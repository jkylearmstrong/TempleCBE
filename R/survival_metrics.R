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
    n_test <- nrow(test_df)

    bs_vec <- vapply(seq_along(times), function(t_idx) {
      p_hat_t <- if (is.matrix(surv_mat)) surv_mat[t_idx, ] else rep(surv_mat[t_idx], n_test)
      ipcw_brier_score(times[t_idx], test_obs_time, test_status, p_hat_t, cens_fit)
    }, numeric(1))

    ibs_val <- integrate_brier_score(times, bs_vec)

    tibble::tibble(IBS = unname(ibs_val), lambda = opt_lambda, alpha = cur_alpha)
  }, error = function(e) {
    tibble::tibble(IBS = NA_real_, lambda = NA_real_, alpha = cur_alpha)
  })
}

#' @keywords internal
#' @noRd
censoring_survival_prob <- function(t, cens_fit) {
  if (length(cens_fit$time) == 0 || t < min(cens_fit$time)) return(1.0)
  idx <- findInterval(t, cens_fit$time)
  if (idx == 0) return(1.0)
  p <- cens_fit$surv[idx]
  if (is.na(p) || p <= 0) p <- 0.001
  p
}

#' Inverse-Probability-of-Censoring-Weighted Brier Score at One Time Point
#'
#' Implements the IPCW Brier score of Graf et al. (1999):
#' \code{BS(t) = (1/n) * sum_i[ w_i(t) * (1(T_i > t) - S_hat(t | x_i))^2 ]},
#' where subjects still at risk at \code{t} are weighted \code{1/G(t)} and
#' subjects with an observed event at or before \code{t} are weighted
#' \code{1/G(T_i-)}; subjects censored at or before \code{t} contribute 0.
#' The normalizer is always the full sample size \code{n} — not the sum of
#' the weights that happened to contribute at this \code{t} — so that
#' excluded (censored-before-\code{t}) subjects still count in the
#' denominator like every other subject.
#'
#' @param t A single evaluation time.
#' @param obs_time Numeric vector of observed follow-up times, one per subject.
#' @param status Numeric/integer vector of event indicators (1 = event, 0 = censored), one per subject.
#' @param p_hat Numeric vector of predicted survival probabilities at \code{t}, one per subject.
#' @param cens_fit A \code{\link[survival]{survfit}} object estimating the censoring distribution.
#' @return A single numeric Brier score.
#' @keywords internal
#' @noRd
ipcw_brier_score <- function(t, obs_time, status, p_hat, cens_fit) {
  n <- length(obs_time)
  G_t <- censoring_survival_prob(t, cens_fit)

  at_risk <- obs_time > t
  sq_err_sum <- sum((1 / G_t) * (1 - p_hat[at_risk])^2)

  event_by_t <- obs_time <= t & status == 1
  if (any(event_by_t)) {
    G_yi <- vapply(obs_time[event_by_t] - 1e-5, censoring_survival_prob, numeric(1), cens_fit = cens_fit)
    sq_err_sum <- sq_err_sum + sum((1 / G_yi) * (0 - p_hat[event_by_t])^2)
  }

  sq_err_sum / n
}

#' Trapezoidal Integrated Brier Score
#'
#' Integrates a vector of time-point Brier scores over their time grid and
#' normalizes by the grid's range, giving the average Brier score over the
#' observed follow-up window.
#'
#' @param times Numeric vector of evaluation times.
#' @param bs_vec Numeric vector of Brier scores, one per element of \code{times}.
#' @return A single numeric integrated Brier score.
#' @keywords internal
#' @noRd
integrate_brier_score <- function(times, bs_vec) {
  valid_idx <- !is.na(bs_vec)
  times_v <- times[valid_idx]
  bs_v <- bs_vec[valid_idx]

  if (length(times_v) < 2) {
    return(mean(bs_v, na.rm = TRUE))
  }

  t_diff <- diff(times_v)
  bs_mid <- (bs_v[-length(bs_v)] + bs_v[-1]) / 2
  total_t <- max(times_v) - min(times_v)
  if (total_t > 0) sum(bs_mid * t_diff) / total_t else mean(bs_v, na.rm = TRUE)
}
