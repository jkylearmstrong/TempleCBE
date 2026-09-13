#' Cross-Validate a Penalized Cox Model With yardstick Survival Metrics
#'
#' A tidymodels counterpart to [glmnet::cv.glmnet()] for right-censored and
#' start/stop survival data. Every combination of `mixture` and `penalty` is
#' fit on each analysis set and scored on the assessment set with a
#' \pkg{yardstick} survival metric set, and the best settings are chosen by one
#' metric: the integrated Brier score by default.
#'
#' @details
#' **Folds.** Unless `resamples` is given, folds come from
#' [rsample::group_vfold_cv()] grouped by `group` (default: `subject_id`), so a
#' subject's start/stop rows are never split between analysis and assessment
#' sets, as they would be by `cv.glmnet()`'s row-level folds. Group by a
#' coarser unit, such as study site, for leave-sites-out cross-validation;
#' each subject must then belong to one group.
#'
#' **Fitting.** For each `mixture`, one penalty path is computed on all of
#' `data` (as `cv.glmnet()` does) and every fold is fit along it. With a recipe,
#' the recipe is prepped on each analysis set, so preprocessing is learned
#' inside the fold.
#'
#' **Predictions.** Survival for each assessment-set subject is computed from
#' the Breslow baseline hazard of the analysis set. With `covariates = "path"`,
#' the cumulative hazard is integrated over the subject's start/stop covariate
#' path, carrying each interval's values forward to the next interval and past
#' the last one; with `"baseline"`, only the covariates of the first interval
#' are used, which uses no information from after time 0. The static
#' concordance metric uses predicted survival at the last `eval_time`.
#'
#' **Scoring.** Start/stop rows are collapsed to one row per subject
#' ([surv_subject_truth()]), weighted for censoring with Graf weights from the
#' analysis set ([graf_weights()]), and passed to `metrics`.
#'
#' **Selection.** `lambda.min` is the penalty, across all `mixture` values,
#' with the best mean `metric` over folds (for time-specific metrics, at the
#' first `eval_time`). `lambda.1se` is the largest penalty, at the same
#' mixture, whose mean is within one standard error of that best mean.
#'
#' @param x A data frame or matrix of predictors, or a [recipes::recipe()]
#'   whose outcome is a single `Surv` column.
#' @param y For the data frame and matrix methods, a [survival::Surv()]
#'   outcome with one element per row of `x`.
#' @param formula A formula with a `Surv()` outcome, as in [coxnet()].
#' @param data A data frame with the outcome, predictors, `subject_id`, and
#'   `group` columns.
#' @param subject_id Subject identifier: a column name for the formula and
#'   recipe methods (for a recipe, a single column with role `"id"` is used
#'   when `NULL`), or a vector for the data frame and matrix methods. Required
#'   for start/stop data; for right-censored data, each row is a subject when
#'   `NULL`.
#' @param group Optional grouping for folds, in the same form as
#'   `subject_id`. Defaults to `subject_id`.
#' @param mixture Numeric vector of elastic-net mixing values to try.
#' @param penalty Optional decreasing penalty path; by default glmnet's path
#'   for each `mixture`.
#' @param resamples An `rset` of `data`, such as [rsample::group_vfold_cv()].
#'   Overrides `v` and `group`.
#' @param v Number of folds when `resamples` is `NULL`.
#' @param metrics A [yardstick::metric_set()] of survival metrics. Defaults to
#'   the integrated Brier score, concordance, time-specific Brier score, and
#'   time-dependent ROC AUC.
#' @param eval_time Evaluation times. Defaults to deciles (10th to 90th
#'   percentile) of the observed event times.
#' @param metric Name of the metric used to choose settings; the first metric
#'   in `metrics` by default.
#' @param covariates `"path"` or `"baseline"`; see Details.
#' @param trunc Lower bound for the censoring probability in the Graf weights.
#' @param parallel If `TRUE`, fit folds with [furrr::future_map()]; set a
#'   [future::plan()] first.
#' @param ... Further arguments passed to [glmnet::glmnet()], such as
#'   `cox.ties` or `nlambda`.
#' @return A `cv_coxnet` object, with:
#'   * `metrics`: mean, `n`, and `std_err` of each metric by `mixture`,
#'     `penalty`, and `.eval_time`;
#'   * `fold_metrics`: every fold's metrics;
#'   * `best_by_mixture`: `lambda_min` and `lambda_1se` for each `mixture`;
#'   * `mixture`, `lambda_min`, `lambda_1se`, `metric`, `direction`;
#'   * `fit`: a [coxnet()] model fit to all of `data` at the chosen `mixture`,
#'     with `lambda_min` as its default penalty;
#'   * `eval_time`, `resamples`, and `covariates`.
#'
#'   Use `predict()`, `tidy()`, `autoplot()`, and `tune::collect_metrics()` on
#'   it.
#' @seealso [coxnet()], [nested_cv_coxnet()], [surv_subject_truth()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("rsample", quietly = TRUE) &&
#'     requireNamespace("yardstick", quietly = TRUE)) {
#'   set.seed(1)
#'   # Start/stop data: up to 3 visits per subject, events only at the last one
#'   long <- do.call(rbind, lapply(1:80, function(i) {
#'     k <- sample(1:3, 1)
#'     stops <- cumsum(stats::runif(k, 2, 6))
#'     risk <- stats::rnorm(1)
#'     data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
#'                status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
#'                x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
#'   }))
#'   cv <- cv_coxnet(
#'     survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
#'     data = long, subject_id = "subject_id", mixture = c(0.5, 1),
#'     v = 5, nlambda = 20, cox.ties = "breslow"
#'   )
#'   cv
#'   generics::tidy(cv, penalty = "lambda.1se")
#' }
#' }
cv_coxnet <- function(x, ...) {
  UseMethod("cv_coxnet")
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.default <- function(x, ...) {
  stop(
    "`cv_coxnet()` is not defined for a '", class(x)[1], "'. Pass a data frame or matrix ",
    "with `y`, a formula with `data`, or a recipe with `data`.",
    call. = FALSE
  )
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.data.frame <- function(x, y, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                                 resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                                 metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                                 parallel = FALSE, ...) {
  cv_coxnet_impl(
    spec_xy(x, y, subject_id, group),
    mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
    eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
    parallel = parallel, ...
  )
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.matrix <- function(x, y, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                             resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                             metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                             parallel = FALSE, ...) {
  cv_coxnet_impl(
    spec_xy(matrix_to_df(x), y, subject_id, group),
    mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
    eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
    parallel = parallel, ...
  )
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.formula <- function(formula, data, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                              resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                              metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                              parallel = FALSE, ...) {
  cv_coxnet_impl(
    spec_formula(formula, data, subject_id, group),
    mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
    eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
    parallel = parallel, ...
  )
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.recipe <- function(x, data, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                             resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                             metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                             parallel = FALSE, ...) {
  cv_coxnet_impl(
    spec_recipe(x, data, subject_id, group),
    mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
    eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
    parallel = parallel, ...
  )
}

#' Use a `cv_coxnet` Result
#'
#' Predict from, tidy, plot, or collect the metrics of a [cv_coxnet()] result.
#' Predictions and coefficients come from the model refit to all the data at
#' the chosen `mixture`.
#'
#' @param object,x A [cv_coxnet()] result.
#' @param new_data A data frame of new predictors.
#' @param type,eval_time,increasing As for [predict.coxnet_model()].
#' @param penalty `"lambda.min"` (default), `"lambda.1se"`, or a number.
#' @param summarize For `collect_metrics()`: `TRUE` for means over resamples,
#'   `FALSE` for each resample's metrics.
#' @param ... Not used.
#' @return `predict()`: a tibble as from [predict.coxnet_model()]. `tidy()`:
#'   the coefficients at `penalty`. `collect_metrics()`: a tibble of metrics.
#'   `autoplot()`: a ggplot of the selection metric against the penalty, with
#'   `lambda.min` (solid) and `lambda.1se` (dashed) marked.
#' @name cv_coxnet-methods
#' @export
predict.cv_coxnet <- function(object, new_data, type = c("linear_pred", "survival"),
                              penalty = "lambda.min", eval_time = NULL, increasing = TRUE, ...) {
  stats::predict(
    object$fit, new_data,
    type = match.arg(type), penalty = cv_penalty(object, penalty),
    eval_time = eval_time, increasing = increasing
  )
}

#' @rdname cv_coxnet-methods
#' @exportS3Method generics::tidy
tidy.cv_coxnet <- function(x, penalty = "lambda.min", ...) {
  tidy.coxnet_model(x$fit, penalty = cv_penalty(x, penalty))
}

#' @rdname cv_coxnet-methods
#' @exportS3Method tune::collect_metrics
collect_metrics.cv_coxnet <- function(x, ..., summarize = TRUE) {
  if (isTRUE(summarize)) x$metrics else x$fold_metrics
}

#' @rdname cv_coxnet-methods
#' @exportS3Method ggplot2::autoplot
autoplot.cv_coxnet <- function(object, ...) {
  m <- object$metrics[object$metrics$.metric == object$metric, , drop = FALSE]
  if (!is.na(object$metric_eval_time)) {
    m <- m[!is.na(m$.eval_time) & m$.eval_time == object$metric_eval_time, , drop = FALSE]
  }
  m$mixture <- factor(m$mixture)
  p <- ggplot2::ggplot(m, ggplot2::aes(x = .data$penalty, y = .data$mean, colour = .data$mixture)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$mean - .data$std_err, ymax = .data$mean + .data$std_err),
      width = 0, na.rm = TRUE
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = object$lambda_min, linetype = "solid", colour = temple_hex[["black"]]) +
    ggplot2::geom_vline(xintercept = object$lambda_1se, linetype = "dashed", colour = temple_hex[["black"]]) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Penalty (lambda, log scale)", y = object$metric, colour = "Mixture",
      caption = "Solid line: lambda.min; dashed line: lambda.1se"
    )
  if (nlevels(m$mixture) <= length(temple_palettes$main)) p <- p + scale_colour_temple()
  p
}

#' @export
print.cv_coxnet <- function(x, ...) {
  folds <- nrow(x$resamples)
  cat("<cv_coxnet> penalized Cox model,", folds, "resamples\n")
  cat("  metric:", x$metric, paste0("(", x$direction, ")"),
      if (!is.na(x$metric_eval_time)) paste("at eval_time", format(x$metric_eval_time)) else "", "\n")
  cat("  mixture:", format(x$mixture),
      "  lambda.min:", format(x$lambda_min, digits = 4),
      "  lambda.1se:", format(x$lambda_1se, digits = 4), "\n")
  cat("  mean at lambda.min:", format(x$best$mean, digits = 4),
      paste0("(std. error ", format(x$best$std_err, digits = 3), ")"), "\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Implementation
# ---------------------------------------------------------------------------

cv_coxnet_impl <- function(spec, data = spec$data, mixture = 1, penalty = NULL, resamples = NULL,
                           v = 10, metrics = NULL, eval_time = NULL, metric = NULL,
                           covariates = c("path", "baseline"), trunc = 0.05, parallel = FALSE, ...) {
  rlang::check_installed(
    c("glmnet", "survival", "rsample", "yardstick"),
    reason = "to cross-validate `coxnet()` models."
  )
  covariates <- match.arg(covariates)
  mixture <- check_mixture(mixture)
  path_user <- check_path(penalty)
  check_trunc(trunc)
  glmnet_args <- check_glmnet_args(list(...))
  metrics <- metrics %||% default_surv_metrics()
  info <- surv_metric_info(metrics)
  metric <- metric %||% info$metric[1]
  if (!is.character(metric) || length(metric) != 1 || !metric %in% info$metric) {
    stop("`metric` must be one of the metrics in `metrics`: ", paste(info$metric, collapse = ", "), ".", call. = FALSE)
  }

  check_subject_nesting(data, spec)
  full <- spec$fit_frame(data)
  check_coxnet_data(full$x, full$y)
  if (is.null(spec$subject_col) && identical(attr(full$y, "type"), "counting")) {
    stop(
      "`subject_id` is required for start/stop Surv(start, stop, event) outcomes, ",
      "so each subject's intervals stay in one fold and are scored together.",
      call. = FALSE
    )
  }
  truth_full <- surv_subject_truth(full$y, subject_keys(full$y, subject_values(data, spec)))
  if (any(truth_full$.entry > 0)) {
    warning(
      "Some subjects enter after time 0. Survival is predicted from time 0 and the Graf ",
      "censoring weights don't correct for delayed entry.",
      call. = FALSE
    )
  }
  eval_time <- eval_time %||% default_eval_time(truth_full$.truth)
  needs_two <- any(info$class == "integrated_survival_metric")
  check_eval_time(eval_time, min_length = if (needs_two) 2 else 1)

  paths <- lapply(mixture, function(m) {
    path_user %||% fit_glmnet_cox(full$x, full$y, m, NULL, glmnet_args)$lambda
  })

  resamples <- resamples %||% make_coxnet_folds(data, spec, v)
  if (!inherits(resamples, "rset")) {
    stop("`resamples` must be an rsample rset, such as rsample::group_vfold_cv().", call. = FALSE)
  }
  if (nrow(resamples$splits[[1]]$data) != nrow(data)) {
    stop("`resamples` must be built on `data`.", call. = FALSE)
  }

  score_split <- function(split) {
    analysis <- rsample::analysis(split)
    assessment <- rsample::assessment(split)
    train <- spec$fit_frame(analysis)
    test <- spec$new_frame(assessment, train$blueprint)
    # Keys, not raw identifiers: a bootstrap analysis set repeats subjects.
    subject_test <- subject_keys(test$y, subject_values(assessment, spec))
    truth_train <- surv_subject_truth(train$y, subject_keys(train$y, subject_values(analysis, spec)))
    truth_test <- surv_subject_truth(test$y, subject_test)
    weights <- graf_weights(truth_test$.truth, eval_time, censoring_km(truth_train$.truth), trunc)

    purrr::list_rbind(purrr::map2(mixture, paths, function(m, path) {
      fit <- tryCatch(fit_glmnet_cox(train$x, train$y, m, path, glmnet_args), error = function(e) e)
      if (inherits(fit, "error")) {
        warning("glmnet failed on a resample at mixture ", m, ": ", conditionMessage(fit), call. = FALSE)
        return(NULL)
      }
      res <- score_coxnet_path(
        fit, train$x, train$y, test$x, test$y, subject_test,
        penalty = path, eval_time = eval_time, truth = truth_test, weights = weights,
        metrics = metrics, info = info, covariates = covariates
      )
      res$mixture <- m
      res
    }))
  }

  results <- if (isTRUE(parallel)) {
    rlang::check_installed(c("furrr", "future"), reason = "to fit folds in parallel.")
    furrr::future_map(resamples$splits, score_split, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(resamples$splits, score_split)
  }

  fold_ids <- resample_ids(resamples)
  fold_metrics <- purrr::list_rbind(Map(function(res, id) {
    if (is.null(res) || !nrow(res)) return(NULL)
    res$id <- id
    res
  }, results, fold_ids))
  if (is.null(fold_metrics) || !nrow(fold_metrics)) {
    stop("No resample could be fit; see the warnings.", call. = FALSE)
  }
  fold_metrics <- dplyr::select(
    fold_metrics, "id", "mixture", "penalty", ".metric", ".estimator", ".eval_time", ".estimate"
  )

  summary <- summarize_fold_metrics(fold_metrics, c("mixture", "penalty"))
  dynamic <- info$class[info$metric == metric] == "dynamic_survival_metric"
  direction <- info$direction[info$metric == metric]
  metric_eval_time <- if (dynamic) eval_time[1] else NA_real_
  selection <- select_coxnet_penalty(summary, metric, direction, metric_eval_time)
  best_by_mixture <- purrr::list_rbind(lapply(unique(summary$mixture), function(m) {
    s <- tryCatch(
      select_coxnet_penalty(summary[summary$mixture == m, ], metric, direction, metric_eval_time),
      error = function(e) NULL
    )
    if (is.null(s)) return(NULL)
    tibble::tibble(
      mixture = m, lambda_min = s$lambda_min, lambda_1se = s$lambda_1se,
      mean = s$best$mean, std_err = s$best$std_err
    )
  }))

  chosen <- match(selection$mixture, mixture)
  final <- fit_glmnet_cox(full$x, full$y, selection$mixture, paths[[chosen]], glmnet_args)

  structure(
    list(
      metrics = summary,
      fold_metrics = fold_metrics,
      best_by_mixture = best_by_mixture,
      best = selection$best,
      mixture = selection$mixture,
      lambda_min = selection$lambda_min,
      lambda_1se = selection$lambda_1se,
      metric = metric,
      direction = direction,
      metric_eval_time = metric_eval_time,
      eval_time = eval_time,
      covariates = covariates,
      fit = new_coxnet(final, selection$lambda_min, selection$mixture, full$x, full$y, full$blueprint),
      resamples = resamples
    ),
    class = "cv_coxnet"
  )
}

#' Score a glmnet Cox Fit Along a Penalty Path on New Data
#'
#' @return A tibble of yardstick results with a `penalty` column.
#' @keywords internal
#' @noRd
score_coxnet_path <- function(fit, x_train, y_train, x_new, y_new, subject_new, penalty,
                              eval_time, truth, weights, metrics, info, covariates) {
  train <- surv_components(y_train)
  bh <- breslow_cumhaz(coxnet_link(fit, x_train, penalty), train$start, train$stop, train$status)
  new <- surv_components(y_new)
  pred <- predict_subject_survival(
    coxnet_link(fit, x_new, penalty), new$start, new$stop, subject_new, eval_time, bh, covariates
  )
  rows <- match(truth$.subject_id, pred$id)
  frame <- surv_metric_frame(truth$.truth, pred$surv[rows, , , drop = FALSE], eval_time, weights, penalty)
  compute_surv_metrics(frame, metrics, info)
}

#' Long Data Frame of Subject Predictions, One Block per Penalty, for yardstick
#' @keywords internal
#' @noRd
surv_metric_frame <- function(truth, surv, eval_time, weights, penalty) {
  n <- dim(surv)[1]
  n_times <- dim(surv)[2]
  n_pen <- dim(surv)[3]
  weights <- unname(weights)
  new_tbl <- function(cols) vctrs::new_data_frame(cols, class = c("tbl_df", "tbl"))
  preds <- vector("list", n * n_pen)
  pred_time <- numeric(n * n_pen)
  for (l in seq_len(n_pen)) {
    s <- matrix(surv[, , l], nrow = n, ncol = n_times)
    offset <- (l - 1) * n
    for (i in seq_len(n)) {
      preds[[offset + i]] <- new_tbl(list(
        .eval_time = eval_time, .pred_survival = s[i, ], .weight_censored = weights[i, ]
      ))
    }
    pred_time[offset + seq_len(n)] <- s[, n_times]
  }
  tibble::tibble(
    penalty = rep(penalty, each = n),
    .truth = truth[rep(seq_len(n), n_pen)],
    .pred = preds,
    .pred_time = pred_time
  )
}

compute_surv_metrics <- function(frame, metrics, info) {
  grouped <- dplyr::group_by(frame, .data$penalty)
  args <- list(grouped, truth = ".truth")
  if (any(info$class %in% c("dynamic_survival_metric", "integrated_survival_metric"))) {
    args <- c(args, list(".pred"))
  }
  if (any(info$class == "static_survival_metric")) {
    args$estimate <- ".pred_time"
  }
  res <- do.call(metrics, args)
  if (!".eval_time" %in% names(res)) res$.eval_time <- NA_real_
  res
}

default_surv_metrics <- function() {
  rlang::check_installed("yardstick", reason = "for survival metrics.")
  yardstick::metric_set(
    yardstick::brier_survival_integrated,
    yardstick::concordance_survival,
    yardstick::brier_survival,
    yardstick::roc_auc_survival
  )
}

surv_metric_info <- function(metrics) {
  if (!inherits(metrics, "metric_set")) {
    stop("`metrics` must be a yardstick::metric_set().", call. = FALSE)
  }
  info <- tibble::as_tibble(metrics)
  survival_classes <- c("integrated_survival_metric", "dynamic_survival_metric", "static_survival_metric")
  bad <- !info$class %in% survival_classes
  if (any(bad)) {
    stop(
      "`metrics` may only contain survival metrics (integrated, dynamic, or static); not: ",
      paste(info$metric[bad], collapse = ", "), ".",
      call. = FALSE
    )
  }
  info
}

summarize_fold_metrics <- function(fold_metrics, keys) {
  grouped <- dplyr::group_by(
    fold_metrics,
    dplyr::across(dplyr::all_of(c(keys, ".metric", ".estimator", ".eval_time")))
  )
  out <- dplyr::summarise(
    grouped,
    mean = if (all(is.na(.data$.estimate))) NA_real_ else mean(.data$.estimate, na.rm = TRUE),
    n = sum(!is.na(.data$.estimate)),
    std_err = stats::sd(.data$.estimate, na.rm = TRUE) / sqrt(sum(!is.na(.data$.estimate))),
    .groups = "drop"
  )
  out$std_err[out$n < 2] <- NA_real_
  out
}

#' Choose lambda.min and lambda.1se From Summarized Metrics
#'
#' @param summary Output of `summarize_fold_metrics()`.
#' @param metric,direction The selection metric and `"minimize"`/`"maximize"`.
#' @param eval_time For time-specific metrics, the evaluation time to select
#'   on; `NA` otherwise.
#' @return A list with `best` (the winning row), `mixture`, `lambda_min`, and
#'   `lambda_1se`. Ties in the mean go to the larger penalty.
#' @keywords internal
#' @noRd
select_coxnet_penalty <- function(summary, metric, direction, eval_time = NA_real_) {
  cand <- summary[summary$.metric == metric & !is.na(summary$mean), , drop = FALSE]
  if (!is.na(eval_time)) {
    cand <- cand[!is.na(cand$.eval_time) & cand$.eval_time == eval_time, , drop = FALSE]
  }
  if (!nrow(cand)) {
    stop("No resample produced a value for `", metric, "`.", call. = FALSE)
  }
  sign <- if (direction == "minimize") 1 else -1
  best <- cand[order(sign * cand$mean, -cand$penalty)[1], , drop = FALSE]
  same <- cand[cand$mixture == best$mixture, , drop = FALSE]
  se <- if (is.na(best$std_err)) 0 else best$std_err
  within <- if (direction == "minimize") same$mean <= best$mean + se else same$mean >= best$mean - se
  list(best = best, mixture = best$mixture, lambda_min = best$penalty, lambda_1se = max(same$penalty[within]))
}

cv_penalty <- function(object, penalty) {
  if (is.character(penalty)) {
    penalty <- switch(
      match.arg(penalty, c("lambda.min", "lambda.1se")),
      lambda.min = object$lambda_min,
      lambda.1se = object$lambda_1se
    )
  }
  check_single_penalty(penalty)
}

make_coxnet_folds <- function(data, spec, v) {
  if (is.null(spec$group_col)) {
    rsample::vfold_cv(data, v = v)
  } else {
    rsample::group_vfold_cv(data, group = dplyr::all_of(spec$group_col), v = v)
  }
}

resample_ids <- function(resamples) {
  id_cols <- grep("^id[0-9]*$", names(resamples), value = TRUE)
  do.call(paste, c(unclass(resamples[id_cols]), sep = "/"))
}

subject_values <- function(data, spec) {
  if (is.null(spec$subject_col)) seq_len(nrow(data)) else data[[spec$subject_col]]
}

check_subject_nesting <- function(data, spec) {
  if (is.null(spec$subject_col) || is.null(spec$group_col) || identical(spec$subject_col, spec$group_col)) {
    return(invisible(TRUE))
  }
  groups_per_subject <- tapply(data[[spec$group_col]], data[[spec$subject_col]], function(g) length(unique(g)))
  if (any(groups_per_subject > 1)) {
    stop(
      "Each subject must belong to a single `group`; subject(s) in several: ",
      format_ids(names(groups_per_subject)[groups_per_subject > 1]), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Interfaces: each "spec" says how to turn rows of `data` into a predictor
# matrix and Surv outcome, learning preprocessing on analysis rows only.
# ---------------------------------------------------------------------------

spec_xy <- function(x, y, subject_id, group) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or matrix of predictors.", call. = FALSE)
  }
  if (!inherits(y, "Surv")) {
    stop("`y` must be a survival::Surv() object.", call. = FALSE)
  }
  n <- nrow(x)
  if (NROW(y) != n) {
    stop("`y` must have one element per row of `x`.", call. = FALSE)
  }
  pred_cols <- names(x)
  reserved <- intersect(pred_cols, c(".outcome", ".subject_id", ".group"))
  if (length(reserved)) {
    stop("Rename predictor column(s) ", paste(reserved, collapse = ", "), ".", call. = FALSE)
  }
  data <- as.data.frame(x)
  data[[".outcome"]] <- y
  subject_col <- NULL
  group_col <- NULL
  if (!is.null(subject_id)) {
    if (length(subject_id) != n) stop("`subject_id` must have one value per row of `x`.", call. = FALSE)
    data[[".subject_id"]] <- subject_id
    subject_col <- ".subject_id"
  }
  if (!is.null(group)) {
    if (length(group) != n) stop("`group` must have one value per row of `x`.", call. = FALSE)
    data[[".group"]] <- group
    group_col <- ".group"
  }
  list(
    data = data,
    subject_col = subject_col,
    group_col = group_col %||% subject_col,
    fit_frame = function(d) {
      m <- mold_xy(d[pred_cols])
      list(x = predictors_matrix(m$predictors), y = d[[".outcome"]], blueprint = m$blueprint)
    },
    new_frame = function(d, blueprint) {
      f <- hardhat::forge(d[pred_cols], blueprint)
      list(x = predictors_matrix(f$predictors), y = d[[".outcome"]])
    }
  )
}

spec_formula <- function(formula, data, subject_id, group) {
  check_id_columns(data, subject_id, group)
  drop <- unique(c(subject_id, group))
  model_data <- function(d) d[setdiff(names(d), drop)]
  list(
    data = data,
    subject_col = subject_id,
    group_col = group %||% subject_id,
    fit_frame = function(d) {
      m <- hardhat::mold(formula, model_data(d), blueprint = coxnet_formula_blueprint())
      list(x = predictors_matrix(m$predictors), y = surv_outcome(m$outcomes), blueprint = m$blueprint)
    },
    new_frame = function(d, blueprint) {
      f <- hardhat::forge(model_data(d), blueprint, outcomes = TRUE)
      list(x = predictors_matrix(f$predictors), y = surv_outcome(f$outcomes))
    }
  )
}

spec_recipe <- function(recipe, data, subject_id, group) {
  if (is.null(subject_id)) {
    ids <- recipe$var_info$variable[recipe$var_info$role %in% "id"]
    if (length(ids) > 1) {
      stop(
        "Several columns have role \"id\" (", paste(ids, collapse = ", "), "); pass `subject_id`.",
        call. = FALSE
      )
    }
    if (length(ids) == 1) subject_id <- ids
  }
  check_id_columns(data, subject_id, group)
  check_not_predictor <- function(predictors) {
    used <- intersect(c(subject_id, group), names(predictors))
    if (length(used)) {
      stop(
        "`", used[1], "` is a predictor in the recipe; give it another role, ",
        "e.g. `update_role(", used[1], ", new_role = \"id\")`.",
        call. = FALSE
      )
    }
  }
  list(
    data = data,
    subject_col = subject_id,
    group_col = group %||% subject_id,
    fit_frame = function(d) {
      m <- hardhat::mold(recipe, d)
      check_not_predictor(m$predictors)
      list(x = predictors_matrix(m$predictors), y = surv_outcome(m$outcomes), blueprint = m$blueprint)
    },
    new_frame = function(d, blueprint) {
      f <- hardhat::forge(d, blueprint, outcomes = TRUE)
      list(x = predictors_matrix(f$predictors), y = surv_outcome(f$outcomes))
    }
  )
}

make_coxnet_spec <- function(preprocessor, data, subject_id, group) {
  if (inherits(preprocessor, "formula")) {
    spec_formula(preprocessor, data, subject_id, group)
  } else if (inherits(preprocessor, "recipe")) {
    spec_recipe(preprocessor, data, subject_id, group)
  } else {
    stop("`preprocessor` must be a formula or a recipe.", call. = FALSE)
  }
}

check_id_columns <- function(data, subject_id, group) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (arg in list(list("subject_id", subject_id), list("group", group))) {
    value <- arg[[2]]
    if (is.null(value)) next
    if (!is.character(value) || length(value) != 1 || !value %in% names(data)) {
      stop("`", arg[[1]], "` must be the name of a column in `data`.", call. = FALSE)
    }
  }
  invisible(TRUE)
}
