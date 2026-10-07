#' Leave-One-Covariate-Out Inference with MiniPatch Ensembles (LOCO-MP) for Cox Models
#'
#' Implements the LOCO-MP statistical inference framework (Gan, Zheng, & Allen 2022)
#' for penalized Cox proportional hazards regression. Combines random minipatch
#' ensembling (subsampling both observations/subjects and covariates) with
#' out-of-bag Leave-One-Covariate-Out evaluation to produce distribution-free
#' feature importance estimates, standard errors, asymptotic z-tests, and
#' confidence intervals for survival models.
#'
#' @details
#' **Tests and intervals.** For each predictor, `importance` is the mean, over
#' the subjects that were out of bag in patches both with and without it, of the
#' excess IPCW integrated Brier loss when it is left out. `p_value` is from a
#' \emph{one-sided} z test of `importance > 0`, the upper tail of the standard
#' normal at `statistic`, and `p_adjusted` adjusts those one-sided p-values with
#' `p_adjust`. `conf_low` and `conf_high` are a \emph{two-sided} interval at
#' level `1 - alpha`, not adjusted for multiple testing. The p-values and the
#' interval therefore answer different questions: the interval excludes 0 when
#' the unadjusted one-sided p-value is below `alpha / 2`, whereas
#' `p_adjusted < alpha` is a one-sided, adjusted criterion. (With
#' `p_adjust = "none"`, a predictor whose `p_value` is between `alpha / 2` and
#' `alpha` passes the test but has an interval that still contains 0.)
#' [autoplot()][autoplot.cbe_loco_mp_coxnet] colours a predictor "Significant"
#' by `p_adjusted < alpha`, that is, by the one-sided adjusted test, not by
#' whether its interval excludes 0.
#'
#' **Data.** The predictors must be numeric, complete, and finite, and there
#' must be at least 3 of them: each minipatch keeps 2 and leaves at least 1 out.
#' This is checked once before any patch is fit. A minipatch that fails anyway
#' (for example, one whose sampled subjects have no events) is dropped with a
#' warning that gives the count and the first error; `B` in the result is the
#' number that succeeded.
#'
#' @param formula A formula with a \code{Surv()} outcome, e.g. \code{Surv(time, status) ~ x1 + x2},
#'   or a [recipes::recipe()] whose outcome is a \code{Surv} column. A formula may not use
#'   `strata()` or `offset()`; see [coxnet()].
#' @param data A data frame containing the variables in the model.
#' @param x An optional predictor matrix (used if \code{formula} and \code{data} are \code{NULL}).
#' @param y An optional \code{Surv} outcome object (used if \code{formula} and \code{data} are \code{NULL}).
#' @param B Number of minipatches to generate. Default is 100.
#' @param n_ratio Subsampling fraction for subjects/observations without replacement.
#'   Default is 0.7.
#' @param m_ratio Subsampling fraction for predictors without replacement. Default is 0.5.
#' @param penalty Penalty parameter for \code{coxnet}. If \code{NULL}, each minipatch
#'   is predicted at the smallest penalty on its own glmnet path, an almost
#'   unpenalized fit.
#' @param mixture Elastic-net mixing parameter (\eqn{\alpha \in [0, 1]}). Default is 1 (lasso).
#' @param eval_time Numeric vector of evaluation time points for IPCW Brier loss integration.
#'   If \code{NULL}, defaults to deciles of event times.
#' @param subject_id Optional character column name identifying subjects for clustered /
#'   counting-process start/stop data. With a recipe, a single column with role
#'   \code{"id"} is used when \code{NULL}, as in [cv_coxnet()].
#' @param alpha Significance level: the two-sided confidence intervals have level
#'   \code{1 - alpha}, and \code{autoplot()} calls a predictor significant when its
#'   adjusted one-sided p-value is below \code{alpha}. Default is 0.05.
#' @param p_adjust Multiple testing correction method for the one-sided p-values. Options include
#'   \code{"bonferroni"}, \code{"BH"}, \code{"fdr"}, \code{"holm"}, or \code{"none"}.
#'   Default is \code{"bonferroni"}.
#' @param trunc Lower truncation threshold for Kaplan-Meier censoring weights. Default is 0.05.
#' @param parallel Logical; if \code{TRUE} and \code{future} is available, runs patches in parallel.
#' @param seed Optional random seed for reproducible subsampling. The caller's
#'   RNG state is restored on exit.
#' @param covariates \code{"path"} (default) or \code{"baseline"}: how the out-of-bag
#'   survival curve of a subject with start/stop rows uses the covariates, as in
#'   \code{\link{cv_coxnet}}. With \code{"path"} the cumulative hazard is integrated
#'   along the subject's covariate path, so no value from after an evaluation time
#'   predicts survival at it; \code{"baseline"} uses the first interval's covariates.
#'   The two agree for right-censored data.
#' @param ... Additional arguments passed to \code{coxnet()}, and so to
#'   \code{\link[glmnet]{glmnet}}, such as \code{cox.ties}, \code{standardize}, or
#'   \code{penalty.factor}. Not \code{weights} or \code{offset}.
#'
#' @return An S3 object of class \code{"cbe_loco_mp_coxnet"} containing:
#'   \item{results}{A tibble with columns \code{term}, \code{importance}, \code{std_error},
#'     \code{statistic}, \code{p_value} (one-sided, for \code{importance > 0}),
#'     \code{p_adjusted}, \code{conf_low}, \code{conf_high} (a two-sided interval at
#'     level \code{1 - alpha}); see Details.}
#'   \item{eval_time}{Evaluation time grid used for IPCW loss integration.}
#'   \item{B}{Number of successfully fitted minipatches; a warning says when some failed.}
#'   \item{n_ratio, m_ratio}{Subsampling ratios used.}
#'   \item{alpha}{Significance level.}
#'   \item{p_adjust}{Multiple testing adjustment method.}
#'   \item{covariates}{How start/stop covariates were used; see \code{covariates}.}
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
                               covariates = c("path", "baseline"),
                               ...) {
  rlang::check_installed(c("glmnet", "survival"), reason = "for cbe_loco_mp_coxnet().")
  p_adjust <- match.arg(p_adjust)
  covariates <- match.arg(covariates)

  if (!is.null(seed)) local_seed(seed)

  # 1. Resolve inputs
  if (!is.null(formula) && !is.null(data)) {
    spec <- make_coxnet_spec(formula, data, subject_id = subject_id, group = NULL)
    frame <- spec$fit_frame(data)
    x_mat <- frame$x
    y_surv <- frame$y
    # The subject column the spec knows, which is `subject_id` or, for a recipe,
    # the column with role "id" that the spec inferred.
    subjs <- if (!is.null(spec$subject_col)) subject_values(data, spec) else NULL
  } else if (!is.null(x) && !is.null(y)) {
    x_mat <- as.matrix(x)
    if (!is.numeric(x_mat)) {
      stop(
        "`x` must be numeric; use the formula interface, or a recipe with recipes::step_dummy(), ",
        "for factors and characters.",
        call. = FALSE
      )
    }
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

  # Every patch keeps two predictors, which is what `coxnet()` needs, and leaves
  # at least one out, which LOCO needs.
  if (p_total < 3L) {
    stop(
      "At least 3 predictors are required for LOCO-MP feature importance: each minipatch needs 2 ",
      "and leaves at least 1 out; got ", p_total, ".",
      call. = FALSE
    )
  }

  # Validate the data once, here, rather than in every patch: a bad cell would
  # otherwise fail each patch that contains it, and silently shrink the ensemble.
  check_coxnet_data(x_mat, y_surv)
  check_glmnet_args(list(...))

  # Subject-level truth: one row per subject, sorted by subject. Everything
  # subject-indexed below (sampling, weights, outcomes) follows this order, so
  # the result doesn't depend on how the rows of `data` happen to be ordered.
  row_key <- if (!is.null(subjs)) subjs else seq_len(n_total)
  truth_all <- surv_subject_truth(y_surv, subjs)
  subj_ids <- truth_all$.subject_id
  n_subjs <- length(subj_ids)
  if (n_subjs < 4L) {
    stop("LOCO-MP needs at least 4 subjects; got ", n_subjs, ".", call. = FALSE)
  }

  # Default evaluation times
  if (is.null(eval_time)) {
    eval_time <- default_eval_time(truth_all$.truth)
  }
  eval_time <- sort(unique(eval_time))
  check_eval_time(eval_time)
  n_eval <- length(eval_time)

  # Sample sizes for patches. Every patch keeps at least two predictors
  # (`coxnet()` needs two) and leaves at least one out (p_total >= 3, above).
  n_patch <- min(max(5L, as.integer(round(n_ratio * n_subjs))), n_subjs - 1L)
  m_patch <- max(min(2L, p_total - 1L), min(p_total - 1L, as.integer(round(m_ratio * p_total))))

  # Censoring weights and event-free status of each subject at eval_time
  cens_km <- censoring_km(truth_all$.truth)
  weights_mat <- graf_weights(truth_all$.truth, eval_time, censoring = cens_km, trunc = trunc)
  surv_parts <- surv_components(truth_all$.truth)
  status_at_t <- outer(surv_parts$stop, eval_time, ">") * 1

  # 2. Generate and fit minipatches
  fit_one_patch <- function(b) {
    sampled_subjs <- sample(subj_ids, size = n_patch, replace = FALSE)
    in_bag <- row_key %in% sampled_subjs
    idx_in <- which(in_bag)
    idx_oob <- which(!in_bag)

    if (length(idx_oob) == 0L) return(NULL)

    feat_idx <- sort(sample.int(p_total, size = m_patch, replace = FALSE))
    x_train <- x_mat[idx_in, feat_idx, drop = FALSE]
    y_train <- y_surv[idx_in]

    fit <- tryCatch({
      coxnet(x = x_train, y = y_train, mixture = mixture, penalty = penalty, ...)
    }, error = function(e) e)
    if (inherits(fit, "error")) return(patch_failure(fit))

    # Without a `penalty`, use the smallest penalty on the patch's own path:
    # an almost unpenalized fit.
    pen <- penalty %||% fit$penalty %||% utils::tail(fit$fit$lambda, 1)

    # Out-of-bag survival, one row per subject: the patch's Breslow baseline
    # hazard along the subject's covariate path (as in `cv_coxnet()`), so no
    # covariate value from after an evaluation time is used to predict it.
    oob_surv <- tryCatch({
      train <- surv_components(y_train)
      bh <- breslow_cumhaz(coxnet_link(fit$fit, x_train, pen), train$start, train$stop, train$status)
      oob <- surv_components(y_surv[idx_oob])
      predict_subject_survival(
        coxnet_link(fit$fit, x_mat[idx_oob, feat_idx, drop = FALSE], pen),
        oob$start, oob$stop, row_key[idx_oob], eval_time, bh, covariates
      )
    }, error = function(e) e)
    if (inherits(oob_surv, "error")) return(patch_failure(oob_surv))

    list(
      oob_subjs = oob_surv$id,
      feat_idx = feat_idx,
      surv_matrix = matrix(oob_surv$surv, nrow = length(oob_surv$id), ncol = n_eval)
    )
  }

  patches <- if (isTRUE(parallel) && requireNamespace("furrr", quietly = TRUE)) {
    furrr::future_map(seq_len(B), fit_one_patch, .options = furrr::furrr_options(seed = TRUE))
  } else {
    lapply(seq_len(B), fit_one_patch)
  }

  # A patch that failed (as opposed to one with nothing out of bag, which is
  # NULL) is counted and its first error kept, so a shrinking ensemble is said
  # out loud instead of passing for a smaller B.
  failed <- vapply(patches, inherits, logical(1), what = "loco_patch_failure")
  n_failed <- sum(failed)
  first_error <- if (n_failed > 0L) patches[[which(failed)[1]]]$message
  patches <- patches[!failed & !vapply(patches, is.null, logical(1))]
  b_effective <- length(patches)

  if (b_effective < 3L) {
    stop(
      "Fewer than 3 minipatches succeeded",
      if (n_failed > 0L) paste0(" (", n_failed, " of ", B, " failed; the first error was: ", first_error, ")"),
      ". Check data variance and event counts.",
      call. = FALSE
    )
  }
  if (n_failed > 0L) {
    warning(
      n_failed, " of ", B, " minipatches failed and were dropped, so B is ", b_effective,
      ". The first error was: ", first_error,
      call. = FALSE
    )
  }

  # 3. Out-of-bag LOCO contrast for each feature. Stack the patches into a
  # subject x time x patch array (zero where a subject was in the patch's bag,
  # tracked by `oob`), and a feature x patch membership matrix, so each
  # feature's with/without ensembles are matrix products.
  surv_arr <- array(0, c(n_subjs, n_eval, b_effective))
  oob <- matrix(0, n_subjs, b_effective)
  for (k in seq_len(b_effective)) {
    rows <- match(patches[[k]]$oob_subjs, subj_ids)
    surv_arr[rows, , k] <- patches[[k]]$surv_matrix
    oob[rows, k] <- 1
  }
  surv_flat <- matrix(surv_arr, n_subjs * n_eval, b_effective)
  has_feat <- vapply(patches, function(p) seq_len(p_total) %in% p$feat_idx, logical(p_total))

  var_names <- colnames(x_mat)
  delta_t <- diff(eval_time)
  time_range <- max(eval_time) - min(eval_time)
  if (time_range <= 0) time_range <- 1

  na_result <- function(term) {
    tibble::tibble(
      term = term, importance = NA_real_, std_error = NA_real_, statistic = NA_real_,
      p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_
    )
  }

  results_list <- vector("list", p_total)
  for (j in seq_len(p_total)) {
    with_j <- as.numeric(has_feat[j, ])
    without_j <- 1 - with_j
    if (sum(with_j) == 0 || sum(without_j) == 0) {
      results_list[[j]] <- na_result(var_names[j])
      next
    }

    d_vals <- loco_subject_deltas(
      with_j, without_j, surv_flat, oob, weights_mat, status_at_t, delta_t, time_range
    )

    n_d <- length(d_vals)
    if (n_d < 3L) {
      results_list[[j]] <- na_result(var_names[j])
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
      p_adjust = p_adjust,
      covariates = covariates
    ),
    class = "cbe_loco_mp_coxnet"
  )
}

#' A Minipatch That Failed, With Its Error Message
#' @keywords internal
#' @noRd
patch_failure <- function(error) {
  structure(list(message = conditionMessage(error)), class = "loco_patch_failure")
}

#' Per-Subject LOCO Contrasts From Stacked Minipatch Predictions
#'
#' For each subject, the ensemble survival curve over the minipatches that
#' include a feature (`with`) and over those that omit it (`without`), each
#' averaged over the patches for which the subject was out of bag, gives the
#' subject's IPCW Brier loss integrated over the evaluation times (trapezoidal
#' rule, divided by the time range); the contrast is `without` minus `with`.
#' Subjects out of bag in no patch of either kind are dropped.
#'
#' @param with,without Numeric 0/1 vectors over patches: patches that include
#'   and that omit the feature.
#' @param surv_flat Matrix of `n_subjs * n_eval` rows and one column per patch:
#'   each patch's predicted survival, subject-major then time (0 where the
#'   subject was in the patch's bag).
#' @param oob Matrix (subjects by patches), 1 where the subject was out of bag.
#' @param weights,alive Matrices (subjects by times) of Graf weights and of
#'   event-free status at each evaluation time.
#' @param delta_t,time_range Gaps between and range of the evaluation times.
#' @return A numeric vector, one contrast per usable subject.
#' @keywords internal
#' @noRd
loco_subject_deltas <- function(with, without, surv_flat, oob, weights, alive, delta_t, time_range) {
  n_subjs <- nrow(oob)
  n_eval <- ncol(weights)
  integrated_loss <- function(member) {
    count <- as.vector(oob %*% member)
    surv <- matrix(surv_flat %*% member, n_subjs, n_eval) / count
    loss <- weights * (alive - surv)^2
    mid <- (loss[, -1, drop = FALSE] + loss[, -n_eval, drop = FALSE]) / 2
    list(loss = as.vector(mid %*% delta_t) / time_range, count = count)
  }
  plus <- integrated_loss(with)
  minus <- integrated_loss(without)
  usable <- plus$count > 0 & minus$count > 0
  (minus$loss - plus$loss)[usable]
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
#'   \code{p_value} and \code{p_adjusted} are one-sided (for \code{importance > 0});
#'   \code{conf_low} and \code{conf_high} are a two-sided interval at level
#'   \code{1 - alpha}. See \code{\link{cbe_loco_mp_coxnet}}.
#' @exportS3Method generics::tidy
tidy.cbe_loco_mp_coxnet <- function(x, ...) {
  x$results
}

#' Autoplot Method for LOCO-MP Feature Importance
#'
#' Generates a ggplot2 forest/lollipop chart displaying Leave-One-Covariate-Out
#' importance scores, confidence intervals, and significance thresholds.
#'
#' The bars are the two-sided \code{1 - alpha} confidence intervals, which are
#' not adjusted for multiple testing, but a predictor is coloured "Significant"
#' when its adjusted \emph{one-sided} p-value is below \code{alpha}, not when
#' its bar excludes 0. The two criteria can disagree in either direction: with
#' \code{p_adjust = "none"}, a "Significant" predictor whose p-value is between
#' \code{alpha / 2} and \code{alpha} has a bar that still crosses 0, and after a
#' correction such as Bonferroni a bar that excludes 0 can be "Not Significant".
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

#' @rdname cbe_loco_mp_coxnet
#' @export
loco_mp_coxnet <- cbe_loco_mp_coxnet

