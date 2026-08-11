#' Integrated Brier Score (IBS) Evaluation for Regularized Survival Models
#'
#' Computes the Integrated Brier Score (IBS) for regularized Cox proportional hazards models
#' fit via cross-validated `glmnet`. Supports resampling split objects (e.g., `rsplit` from `rsample`)
#' as well as data frames.
#'
#' @param object A resampling fold object (e.g. `rsplit` from `rsample`) or a data frame/tibble.
#' @param alpha The elastic net mixing parameter: 1 for Lasso, 0 for Ridge (default 1).
#' @param formula Optional formula specifying target survival response and predictor features.
#' @param cox.ties Method for handling ties in Cox model, passed to `glmnet::cv.glmnet` (default `"efron"`).
#' @return A tibble containing `IBS`, optimal `lambda`, and `alpha`.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("glmnet", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
#'   df <- data.frame(
#'     time = runif(60, 5, 100),
#'     status = rbinom(60, 1, 0.7),
#'     age = rnorm(60, 50, 10),
#'     bmi = rnorm(60, 25, 4)
#'   )
#'   glmnet_IBS(df)
#' }
#' }
glmnet_IBS <- function(object, alpha = 1, formula = NULL, cox.ties = "efron") {
  cur_alpha <- alpha
  
  if (!requireNamespace("glmnet", quietly = TRUE) || !requireNamespace("survival", quietly = TRUE)) {
    stop("Packages 'glmnet' and 'survival' are required for glmnet_IBS evaluation.")
  }
  
  if (is.null(object)) {
    return(tibble::tibble(IBS = NA_real_, lambda = NA_real_, alpha = cur_alpha))
  }
  
  # Extract training (analysis) and testing (assessment) datasets
  if (inherits(object, "rsplit")) {
    if (requireNamespace("rsample", quietly = TRUE)) {
      train_df <- rsample::analysis(object)
      test_df  <- rsample::assessment(object)
    } else {
      stop("Package 'rsample' is required when passing an rsplit object.")
    }
  } else if (is.data.frame(object)) {
    train_df <- object
    test_df  <- object
  } else {
    stop("Input 'object' must be an 'rsplit' object or data frame.")
  }
  
  if (nrow(train_df) == 0 || nrow(test_df) == 0) {
    return(tibble::tibble(IBS = NA_real_, lambda = NA_real_, alpha = cur_alpha))
  }

  tryCatch({
    # Prepare response and predictor matrices
    if (!is.null(formula)) {
      mf_train <- stats::model.frame(formula, data = train_df)
      y_train  <- stats::model.response(mf_train)
      x_train  <- stats::model.matrix(formula, data = train_df)
      if ("(Intercept)" %in% colnames(x_train)) {
        x_train <- x_train[, colnames(x_train) != "(Intercept)", drop = FALSE]
      }
      
      mf_test <- stats::model.frame(formula, data = test_df)
      y_test  <- stats::model.response(mf_test)
      x_test  <- stats::model.matrix(formula, data = test_df)
      if ("(Intercept)" %in% colnames(x_test)) {
        x_test <- x_test[, colnames(x_test) != "(Intercept)", drop = FALSE]
      }
    } else {
      # Auto-detect survival response format
      if (all(c("tstart", "tstop", "status") %in% names(train_df))) {
        y_train <- survival::Surv(train_df$tstart, train_df$tstop, train_df$status)
        y_test  <- survival::Surv(test_df$tstart, test_df$tstop, test_df$status)
        skip_cols <- c("tstart", "tstop", "status", "patient_id", "visit", "id")
      } else if (all(c("time", "status") %in% names(train_df))) {
        y_train <- survival::Surv(train_df$time, train_df$status)
        y_test  <- survival::Surv(test_df$time, test_df$status)
        skip_cols <- c("time", "status", "patient_id", "visit", "id")
      } else {
        stop("Could not automatically identify survival response columns.")
      }
      
      num_cols  <- names(train_df)[sapply(train_df, is.numeric)]
      pred_cols <- setdiff(num_cols, skip_cols)
      
      if (length(pred_cols) == 0) {
        stop("No numeric predictor variables found in dataset.")
      }
      
      x_train <- as.matrix(train_df[, pred_cols, drop = FALSE])
      x_test  <- as.matrix(test_df[, pred_cols, drop = FALSE])
    }

    # Fit regularized Cox model
    cv_fit <- glmnet::cv.glmnet(x_train, y_train, family = "cox", alpha = cur_alpha, cox.ties = cox.ties)
    opt_lambda <- cv_fit$lambda.min

    # Extract predicted survival probability matrix across time points
    sf <- survival::survfit(cv_fit$glmnet.fit, s = opt_lambda, x = x_train, y = y_train, newx = x_test)
    times    <- sf$time
    surv_mat <- sf$surv # rows: times, cols: test subjects

    if (length(times) == 0 || is.null(surv_mat)) {
      return(tibble::tibble(IBS = NA_real_, lambda = opt_lambda, alpha = cur_alpha))
    }

    # Extract test and train event times and status
    if (ncol(y_test) == 3) {
      test_obs_time  <- y_test[, 2]
      test_status    <- y_test[, 3]
      train_obs_time <- y_train[, 2]
      train_status   <- y_train[, 3]
    } else {
      test_obs_time  <- y_test[, 1]
      test_status    <- y_test[, 2]
      train_obs_time <- y_train[, 1]
      train_status   <- y_train[, 2]
    }

    # Kaplan-Meier estimate of censoring distribution on training set
    cens_fit <- survival::survfit(survival::Surv(train_obs_time, 1 - train_status) ~ 1)

    get_cens_prob <- function(t) {
      if (length(cens_fit$time) == 0 || t < min(cens_fit$time)) return(1.0)
      idx <- findInterval(t, cens_fit$time)
      if (idx == 0) return(1.0)
      p <- cens_fit$surv[idx]
      if (is.na(p) || p <= 0) p <- 0.001
      p
    }

    # Compute Brier score BS(t) at each grid time
    bs_vec <- numeric(length(times))
    n_test <- nrow(test_df)

    for (t_idx in seq_along(times)) {
      t <- times[t_idx]
      G_t <- get_cens_prob(t)
      
      w_sum <- 0
      sq_err_sum <- 0
      
      for (i in seq_len(n_test)) {
        y_i <- test_obs_time[i]
        d_i <- test_status[i]
        p_hat <- if (is.matrix(surv_mat)) surv_mat[t_idx, i] else surv_mat[t_idx]
        
        if (y_i > t) {
          w_i <- 1 / G_t
          w_sum <- w_sum + w_i
          sq_err_sum <- sq_err_sum + w_i * (1 - p_hat)^2
        } else if (y_i <= t && d_i == 1) {
          G_yi <- get_cens_prob(y_i - 1e-5)
          w_i <- 1 / G_yi
          w_sum <- w_sum + w_i
          sq_err_sum <- sq_err_sum + w_i * (0 - p_hat)^2
        }
      }
      
      bs_vec[t_idx] <- if (w_sum > 0) sq_err_sum / w_sum else NA_real_
    }

    valid_idx <- !is.na(bs_vec)
    times_v   <- times[valid_idx]
    bs_v      <- bs_vec[valid_idx]

    if (length(times_v) < 2) {
      ibs_val <- mean(bs_v, na.rm = TRUE)
    } else {
      t_diff <- diff(times_v)
      bs_mid <- (bs_v[-length(bs_v)] + bs_v[-1]) / 2
      total_t <- max(times_v) - min(times_v)
      ibs_val <- if (total_t > 0) sum(bs_mid * t_diff) / total_t else mean(bs_v, na.rm = TRUE)
    }

    tibble::tibble(IBS = unname(ibs_val), lambda = opt_lambda, alpha = cur_alpha)
  }, error = function(e) {
    tibble::tibble(IBS = NA_real_, lambda = NA_real_, alpha = cur_alpha)
  })
}
