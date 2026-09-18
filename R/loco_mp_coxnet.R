#' Leave-One-Covariate-Out Inference with MiniPatch Ensembles (LOCO-MP) for Cox Models
#'
#' Implements the LOCO-MP statistical inference framework (Gan, Zheng, & Allen 2022)
#' for penalized Cox proportional hazards regression. Combines random minipatch
#' ensembling (subsampling both observations/subjects and covariates) with
#' out-of-bag Leave-One-Covariate-Out evaluation to produce distribution-free
#' feature importance estimates, standard errors, asymptotic z-tests, and
#' confidence intervals for survival models.
#'
#' @param formula A formula with a \code{Surv()} outcome, e.g. \code{Surv(time, status) ~ x1 + x2}.
#' @param data A data frame containing the variables in the model.
#' @param x An optional predictor matrix (used if \code{formula} and \code{data} are \code{NULL}).
#' @param y An optional \code{Surv} outcome object (used if \code{formula} and \code{data} are \code{NULL}).
#' @param B Number of minipatches to generate. Default is 100.
#' @param n_ratio Subsampling fraction for subjects/observations without replacement.
#'   Default is 0.7.
#' @param m_ratio Subsampling fraction for predictors without replacement. Default is 0.5.
#' @param penalty Penalty parameter for \code{coxnet}. If \code{NULL}, defaults to 0 (unpenalized).
#' @param mixture Elastic-net mixing parameter (\eqn{\alpha \in [0, 1]}). Default is 1 (lasso).
#' @param eval_time Numeric vector of evaluation time points for IPCW Brier loss integration.
#'   If \code{NULL}, defaults to deciles of event times.
#' @param subject_id Optional character column name identifying subjects for clustered /
#'   counting-process start/stop data.
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param p_adjust Multiple testing correction method for p-values. Options include
#'   \code{"bonferroni"}, \code{"BH"}, \code{"fdr"}, \code{"holm"}, or \code{"none"}.
#'   Default is \code{"bonferroni"}.
#' @param trunc Lower truncation threshold for Kaplan-Meier censoring weights. Default is 0.05.
#' @param parallel Logical; if \code{TRUE} and \code{future} is available, runs patches in parallel.
#' @param seed Optional random seed for reproducible subsampling.
#' @param ... Additional arguments passed to \code{coxnet()}.
#'
#' @return An S3 object of class \code{"cbe_loco_mp_coxnet"} containing:
#'   \item{results}{A tibble with columns \code{term}, \code{importance}, \code{std_error},
#'     \code{statistic}, \code{p_value}, \code{p_adjusted}, \code{conf_low}, \code{conf_high}.}
#'   \item{eval_time}{Evaluation time grid used for IPCW loss integration.}
#'   \item{B}{Number of successfully fitted minipatches.}
#'   \item{n_ratio, m_ratio}{Subsampling ratios used.}
#'   \item{alpha}{Significance level.}
#'   \item{p_adjust}{Multiple testing adjustment method.}
#' @seealso [nested_cv_coxnet()], [cv_coxnet()], [coxnet()]
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(42)
#'   n <- 60
#'   df <- data.frame(
#'     time = stats::rexp(n, 0.1) + 0.1,
#'     status = stats::rbinom(n, 1, 0.6),
#'     x1 = stats::rnorm(n),
#'     x2 = stats::rnorm(n),
#'     x3 = stats::rnorm(n)
#'   )
#'   fit <- cbe_loco_mp_coxnet(survival::Surv(time, status) ~ x1 + x2 + x3, data = df, B = 20)
#'   fit
#'   generics::tidy(fit)
#' }
#' }
cbe_loco_mp_coxnet <- function(formula = NULL,
                               data = NULL,
                               x = NULL,
                               y = NULL,
                               B = 100,
                               n_ratio = 0.7,
                               m_ratio = 0.5,
                               penalty = NULL,
                               mixture = 1,
                               eval_time = NULL,
                               subject_id = NULL,
                               alpha = 0.05,
                               p_adjust = c("bonferroni", "BH", "fdr", "holm", "none"),
                               trunc = 0.05,
                               parallel = FALSE,
                               seed = NULL,
                               ...) {
  rlang::check_installed(c("glmnet", "survival"), reason = "for cbe_loco_mp_coxnet().")
  p_adjust <- match.arg(p_adjust)

  if (!is.null(seed)) set.seed(seed)

  # 1. Resolve inputs
  if (!is.null(formula) && !is.null(data)) {
    spec <- make_coxnet_spec(formula, data, subject_id = subject_id, group = NULL)
    frame <- spec$fit_frame(data)
    x_mat <- frame$x
    y_surv <- frame$y
    subjs <- if (!is.null(subject_id)) subject_values(data, spec) else NULL
  } else if (!is.null(x) && !is.null(y)) {
    x_mat <- as.matrix(x)
    if (is.null(colnames(x_mat))) {
      colnames(x_mat) <- paste0("x", seq_len(ncol(x_mat)))
    }
    y_surv <- y
    subjs <- subject_id
  } else {
    stop("Must supply either `formula` and `data` or `x` and `y`.", call. = FALSE)
  }

  p_total <- ncol(x_mat)
  n_total <- nrow(x_mat)

  if (p_total < 2L) {
    stop("At least 2 predictors are required for LOCO-MP feature importance.", call. = FALSE)
  }

  # Unique subject identifiers
  unique_subjs <- if (!is.null(subjs)) unique(subjs) else seq_len(n_total)
  n_subjs <- length(unique_subjs)

  # Default evaluation times and subject-level truth
  truth_all <- surv_subject_truth(y_surv, subjs)
  if (is.null(eval_time)) {
    eval_time <- default_eval_time(truth_all$.truth)
  }
  eval_time <- sort(unique(eval_time))
  check_eval_time(eval_time)
  n_eval <- length(eval_time)

  # Sample sizes for patches
  n_patch <- max(5L, as.integer(round(n_ratio * n_subjs)))
  m_patch <- max(1L, min(p_total - 1L, as.integer(round(m_ratio * p_total))))

  # Precompute censoring KM and Graf weights for collapsed truth
  cens_km <- censoring_km(truth_all$.truth)
  weights_mat <- graf_weights(truth_all$.truth, eval_time, censoring = cens_km, trunc = trunc)

  # Status at eval_times (1 = still event-free / alive at t, 0 = event occurred prior)
  surv_parts <- surv_components(truth_all$.truth)
  status_at_t <- outer(surv_parts$stop, eval_time, ">") * 1

  # Subject mapping: which rows belong to each unique subject
  subj_row_map <- if (!is.null(subjs)) {
    split(seq_len(n_total), subjs)
  } else {
    as.list(seq_len(n_total))
  }

  # 2. Generate and fit minipatches
  fit_one_patch <- function(b) {
    # Subsample subjects
    sampled_subjs <- sample(unique_subjs, size = n_patch, replace = FALSE)
    in_bag <- if (!is.null(subjs)) subjs %in% sampled_subjs else seq_len(n_total) %in% sampled_subjs
    idx_in <- which(in_bag)
    idx_oob <- which(!in_bag)

    if (length(idx_oob) == 0L) return(NULL)

    # Subsample features
    feat_idx <- sort(sample.int(p_total, size = m_patch, replace = FALSE))

    x_train <- x_mat[idx_in, feat_idx, drop = FALSE]
    y_train <- y_surv[idx_in]

    # Fit penalized Cox model
    fit <- tryCatch({
      coxnet(x = x_train, y = y_train, mixture = mixture, penalty = penalty, ...)
    }, error = function(e) NULL)

    if (is.null(fit)) return(NULL)

    # Resolve penalty for prediction if not explicitly provided
    pen <- penalty
    if (is.null(pen)) {
      pen <- if (!is.null(fit$penalty)) fit$penalty else utils::tail(fit$fit$lambda, 1)
    }

    # Predict OOB survival curves
    x_test <- x_mat[idx_oob, feat_idx, drop = FALSE]
    preds <- tryCatch({
      stats::predict(fit, new_data = x_test, type = "survival", eval_time = eval_time, penalty = pen)
    }, error = function(e) NULL)

    if (is.null(preds) || !(".pred" %in% names(preds))) return(NULL)

    # Matrix: rows = idx_oob, cols = eval_time
    s_mat <- do.call(rbind, lapply(preds$.pred, function(df) {
      df <- df[match(eval_time, df$.eval_time), ]
      df$.pred_survival
    }))

    # Identify which unique subjects are OOB in this patch
    oob_subjs <- if (!is.null(subjs)) setdiff(unique_subjs, sampled_subjs) else idx_oob

    # Condense prediction to one row per OOB subject (using last interval for counting process)
    s_subj_mat <- if (!is.null(subjs)) {
      do.call(rbind, lapply(oob_subjs, function(sid) {
        rows_for_s <- which(subjs[idx_oob] == sid)
        # Take the last row of the subject's intervals
        s_mat[utils::tail(rows_for_s, 1), , drop = FALSE]
      }))
    } else {
      s_mat
    }

    list(
      oob_subjs = oob_subjs,
      feat_idx = feat_idx,
      surv_matrix = s_subj_mat
    )
  }

  patches <- if (isTRUE(parallel) && requireNamespace("furrr", quietly = TRUE)) {
    furrr::future_map(seq_len(B), fit_one_patch, .options = furrr::furrr_options(seed = TRUE))
  } else {
    lapply(seq_len(B), fit_one_patch)
  }

  patches <- patches[!vapply(patches, is.null, logical(1))]
  b_effective <- length(patches)

  if (b_effective < 3L) {
    stop("Fewer than 3 minipatches succeeded. Check data variance and event counts.", call. = FALSE)
  }

  # 3. Out-of-bag LOCO Contrast for each feature
  var_names <- colnames(x_mat)
  delta_t <- diff(eval_time)
  time_range <- max(eval_time) - min(eval_time)
  if (time_range <= 0) time_range <- 1

  results_list <- vector("list", p_total)

  for (j in seq_len(p_total)) {
    patches_with_j <- which(vapply(patches, function(p) j %in% p$feat_idx, logical(1)))
    patches_without_j <- which(vapply(patches, function(p) !j %in% p$feat_idx, logical(1)))

    if (length(patches_with_j) == 0L || length(patches_without_j) == 0L) {
      results_list[[j]] <- tibble::tibble(
        term = var_names[j],
        importance = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      )
      next
    }

    # For each unique subject, calculate ensemble prediction with and without j
    d_vals <- numeric(0)

    for (s_idx in seq_along(unique_subjs)) {
      sid <- unique_subjs[s_idx]

      # Find patches containing subject sid in OOB
      with_sub <- patches_with_j[vapply(patches_with_j, function(k) sid %in% patches[[k]]$oob_subjs, logical(1))]
      without_sub <- patches_without_j[vapply(patches_without_j, function(k) sid %in% patches[[k]]$oob_subjs, logical(1))]

      if (length(with_sub) == 0L || length(without_sub) == 0L) next

      # Extract predicted survival vectors for subject sid
      surv_with <- vapply(with_sub, function(k) {
        row_pos <- match(sid, patches[[k]]$oob_subjs)
        patches[[k]]$surv_matrix[row_pos, ]
      }, numeric(n_eval))
      # vapply drops to a plain vector (one value per patch) instead of an
      # n_eval x length(with_sub) matrix only when n_eval == 1; reshape with
      # nrow (not ncol) so rowMeans() still averages across patches per time.
      if (is.vector(surv_with)) surv_with <- matrix(surv_with, nrow = n_eval)
      s_plus <- rowMeans(surv_with)

      surv_without <- vapply(without_sub, function(k) {
        row_pos <- match(sid, patches[[k]]$oob_subjs)
        patches[[k]]$surv_matrix[row_pos, ]
      }, numeric(n_eval))
      if (is.vector(surv_without)) surv_without <- matrix(surv_without, nrow = n_eval)
      s_minus <- rowMeans(surv_without)

      # IPCW Graf Brier loss curve at eval_time
      w_i <- weights_mat[s_idx, ]
      y_i <- status_at_t[s_idx, ]

      loss_plus_t <- w_i * ((y_i - s_plus)^2)
      loss_minus_t <- w_i * ((y_i - s_minus)^2)

      # Integrated loss via trapezoidal rule
      mid_plus <- (loss_plus_t[-1] + loss_plus_t[-n_eval]) / 2
      mid_minus <- (loss_minus_t[-1] + loss_minus_t[-n_eval]) / 2

      l_plus <- sum(delta_t * mid_plus) / time_range
      l_minus <- sum(delta_t * mid_minus) / time_range

      d_i <- l_minus - l_plus
      d_vals <- c(d_vals, d_i)
    }

    n_d <- length(d_vals)
    if (n_d < 3L) {
      results_list[[j]] <- tibble::tibble(
        term = var_names[j],
        importance = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      )
    } else {
      mean_d <- mean(d_vals, na.rm = TRUE)
      sd_d <- stats::sd(d_vals, na.rm = TRUE)
      se_d <- sd_d / sqrt(n_d)
      z_stat <- if (se_d > 0) mean_d / se_d else 0
      p_val <- 1 - stats::pnorm(z_stat) # One-sided test: importance > 0
      z_crit <- stats::qnorm(1 - alpha / 2)

      results_list[[j]] <- tibble::tibble(
        term = var_names[j],
        importance = mean_d,
        std_error = se_d,
        statistic = z_stat,
        p_value = p_val,
        conf_low = mean_d - z_crit * se_d,
        conf_high = mean_d + z_crit * se_d
      )
    }
  }

  res_df <- purrr::list_rbind(results_list)
  res_df$p_adjusted <- stats::p.adjust(res_df$p_value, method = p_adjust)

  # Reorder by importance descending
  res_df <- res_df[order(-res_df$importance), ]

  structure(
    list(
      results = res_df,
      eval_time = eval_time,
      B = b_effective,
      n_ratio = n_ratio,
      m_ratio = m_ratio,
      alpha = alpha,
      p_adjust = p_adjust
    ),
    class = "cbe_loco_mp_coxnet"
  )
}

#' @export
print.cbe_loco_mp_coxnet <- function(x, ...) {
  cat("<cbe_loco_mp_coxnet> Leave-One-Covariate-Out MiniPatch Feature Inference\n")
  cat("  Patches (B):", x$B, "  n_ratio:", x$n_ratio, "  m_ratio:", x$m_ratio, "\n")
  cat("  Multiple testing adjustment:", x$p_adjust, " (alpha = ", x$alpha, ")\n\n", sep = "")

  df_print <- x$results
  df_print$importance <- format(df_print$importance, digits = 4)
  df_print$std_error <- format(df_print$std_error, digits = 4)
  df_print$statistic <- format(df_print$statistic, digits = 3)
  df_print$p_value <- format.pval(df_print$p_value, digits = 3)
  df_print$p_adjusted <- format.pval(df_print$p_adjusted, digits = 3)
  print(as.data.frame(df_print), row.names = FALSE)
  invisible(x)
}

#' Tidy a LOCO-MP Cox Model Object
#'
#' Extracts feature importance and inference metrics into a clean tibble.
#'
#' @param x A \code{cbe_loco_mp_coxnet} object.
#' @param ... Not used.
#' @return A tibble with \code{term}, \code{importance}, \code{std_error},
#'   \code{statistic}, \code{p_value}, \code{p_adjusted}, \code{conf_low}, \code{conf_high}.
#' @exportS3Method generics::tidy
tidy.cbe_loco_mp_coxnet <- function(x, ...) {
  x$results
}

#' Autoplot Method for LOCO-MP Feature Importance
#'
#' Generates a ggplot2 forest/lollipop chart displaying Leave-One-Covariate-Out
#' importance scores, confidence intervals, and significance thresholds.
#'
#' @param object A \code{cbe_loco_mp_coxnet} object.
#' @param ... Additional arguments.
#' @return A ggplot2 plot object.
#' @exportS3Method ggplot2::autoplot
autoplot.cbe_loco_mp_coxnet <- function(object, ...) {
  rlang::check_installed("ggplot2", reason = "for autoplot.cbe_loco_mp_coxnet().")

  df <- object$results
  df <- df[!is.na(df$importance), ]
  if (nrow(df) == 0L) {
    stop("No valid importance estimates to plot.", call. = FALSE)
  }

  df$term <- stats::reorder(df$term, df$importance)
  df$significant <- ifelse(df$p_adjusted < object$alpha, "Significant", "Not Significant")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$importance, y = .data$term, color = .data$significant)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      width = 0.25, linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(
      values = c("Significant" = "#a41e35", "Not Significant" = "gray40"),
      name = paste0("Status (", object$p_adjust, " < ", object$alpha, ")")
    ) +
    ggplot2::labs(
      title = "LOCO-MP Survival Feature Importance",
      subtitle = paste0("B = ", object$B, " MiniPatches (Excess IPCW Integrated Brier Loss when Omitted)"),
      x = expression(paste("LOCO Importance ", Delta[j], " (Excess Brier Loss)")),
      y = "Predictor Variable"
    )

  if (exists("theme_cbe", mode = "function")) {
    p <- p + theme_cbe()
  } else {
    p <- p + ggplot2::theme_minimal()
  }

  p
}
