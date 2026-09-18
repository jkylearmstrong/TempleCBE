#' Nested Cross-Validation of a Penalized Cox Model
#'
#' Estimates how well the whole tuning procedure of [cv_coxnet()] generalizes.
#' For each outer split of an [rsample::nested_cv()] object: `mixture` and
#' `penalty` are chosen by [cv_coxnet()] on that split's inner resamples; the
#' model is refit on the outer analysis set with those settings; and the refit
#' is scored on the outer assessment set, which played no part in tuning.
#'
#' Build `object` with grouped resampling at both levels, e.g.
#' `nested_cv(data, outside = group_vfold_cv(group = subject_id),
#' inside = group_vfold_cv(group = subject_id))`, so a subject's start/stop
#' rows stay together.
#'
#' The outer metrics describe the procedure, not one final model: the chosen
#' settings can differ between outer splits. To fit a final model, run
#' [cv_coxnet()] on all the data.
#'
#' @param object An [rsample::nested_cv()] object.
#' @param preprocessor A formula with a `Surv()` outcome, or a
#'   [recipes::recipe()] whose outcome is a `Surv` column.
#' @param subject_id,group Column names, as in [cv_coxnet()].
#' @param rule Use `"min"` (`lambda.min`) or `"1se"` (`lambda.1se`) from each
#'   inner cross-validation.
#' @param eval_time Evaluation times, shared by all splits. Defaults to deciles
#'   of the event times in the full data.
#' @param importance Method for evaluating feature importance on the outer analysis sets.
#'   Options are `"none"` (default) or `"loco_mp"` (Leave-One-Covariate-Out with MiniPatch ensembles).
#' @inheritParams cv_coxnet
#' @return A tibble, of class `nested_cv_coxnet`, with one row per outer split:
#'   `id`, the chosen `mixture` and `penalty`, `.metrics` (outer assessment-set
#'   metrics), `.coefs` (coefficients of the refit), and `.inner` (the inner
#'   cross-validation's summarized metrics). If `importance = "loco_mp"`, includes
#'   `.importance` with tidy LOCO-MP statistical inference. `tune::collect_metrics()`
#'   averages `.metrics` over outer splits.
#' @seealso [cv_coxnet()], [coxnet()], [cbe_loco_mp_coxnet()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("rsample", quietly = TRUE) &&
#'     requireNamespace("yardstick", quietly = TRUE)) {
#'   set.seed(1)
#'   long <- do.call(rbind, lapply(1:90, function(i) {
#'     k <- sample(1:3, 1)
#'     stops <- cumsum(stats::runif(k, 2, 6))
#'     risk <- stats::rnorm(1)
#'     data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
#'                status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
#'                x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
#'   }))
#'   folds <- rsample::nested_cv(
#'     long,
#'     outside = rsample::group_vfold_cv(group = subject_id, v = 3),
#'     inside = rsample::group_vfold_cv(group = subject_id, v = 3)
#'   )
#'   res <- nested_cv_coxnet(
#'     folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
#'     subject_id = "subject_id", mixture = c(0.5, 1),
#'     metrics = yardstick::metric_set(yardstick::brier_survival_integrated,
#'                                     yardstick::concordance_survival),
#'     nlambda = 20, cox.ties = "breslow"
#'   )
#'   res
#' }
#' }
nested_cv_coxnet <- function(object, preprocessor, subject_id = NULL, group = NULL,
                             rule = c("min", "1se"), mixture = 1, penalty = NULL, metrics = NULL,
                             eval_time = NULL, metric = NULL, covariates = c("path", "baseline"),
                             trunc = 0.05, parallel = FALSE, importance = c("none", "loco_mp"), ...) {
  rlang::check_installed(
    c("glmnet", "survival", "rsample", "yardstick"),
    reason = "for nested cross-validation of `coxnet()` models."
  )
  if (!inherits(object, "nested_cv")) {
    stop("`object` must be an rsample::nested_cv() object.", call. = FALSE)
  }
  rule <- match.arg(rule)
  covariates <- match.arg(covariates)
  importance <- match.arg(importance)
  metrics <- metrics %||% default_surv_metrics()
  info <- surv_metric_info(metrics)

  full_data <- object$splits[[1]]$data
  spec <- make_coxnet_spec(preprocessor, full_data, subject_id, group)
  if (is.null(eval_time)) {
    full <- spec$fit_frame(full_data)
    eval_time <- default_eval_time(
      surv_subject_truth(full$y, subject_keys(full$y, subject_values(full_data, spec)))$.truth
    )
  }

  run_outer <- function(i) {
    outer <- object$splits[[i]]
    analysis <- rsample::analysis(outer)
    assessment <- rsample::assessment(outer)

    inner <- cv_coxnet_impl(
      spec, data = analysis, mixture = mixture, penalty = penalty,
      resamples = object$inner_resamples[[i]], metrics = metrics, eval_time = eval_time,
      metric = metric, covariates = covariates, trunc = trunc, parallel = FALSE, ...
    )
    chosen <- if (rule == "min") inner$lambda_min else inner$lambda_1se
    refit <- inner$fit

    test <- spec$new_frame(assessment, refit$blueprint)
    subject_test <- subject_keys(test$y, subject_values(assessment, spec))
    truth_train <- surv_subject_truth(refit$y, subject_keys(refit$y, subject_values(analysis, spec)))
    truth_test <- surv_subject_truth(test$y, subject_test)
    weights <- graf_weights(truth_test$.truth, eval_time, censoring_km(truth_train$.truth), trunc)

    outer_metrics <- score_coxnet_path(
      refit$fit, refit$x, refit$y, test$x, test$y, subject_test,
      penalty = chosen, eval_time = eval_time, truth = truth_test, weights = weights,
      metrics = metrics, info = info, covariates = covariates
    )
    outer_metrics$penalty <- NULL

    loco_col <- if (importance == "loco_mp") {
      loco_fit <- tryCatch({
        cbe_loco_mp_coxnet(
          formula = preprocessor, data = analysis, subject_id = subject_id,
          mixture = inner$mixture, penalty = chosen, eval_time = eval_time,
          B = 30, trunc = trunc, parallel = FALSE
        )
      }, error = function(e) NULL)
      if (!is.null(loco_fit)) list(generics::tidy(loco_fit)) else list(NULL)
    } else {
      NULL
    }

    res_row <- tibble::tibble(
      id = object$id[i],
      mixture = inner$mixture,
      penalty = chosen,
      .metrics = list(outer_metrics),
      .coefs = list(tidy.coxnet_model(refit, penalty = chosen)),
      .inner = list(inner$metrics)
    )
    if (importance == "loco_mp") {
      res_row$.importance <- loco_col
    }
    res_row
  }

  outer_ids <- seq_along(object$splits)
  rows <- if (isTRUE(parallel)) {
    rlang::check_installed(c("furrr", "future"), reason = "to run outer splits in parallel.")
    furrr::future_map(outer_ids, run_outer, .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(outer_ids, run_outer)
  }

  out <- purrr::list_rbind(rows)
  class(out) <- c("nested_cv_coxnet", class(out))
  out
}

#' @rdname nested_cv_coxnet
#' @param x A `nested_cv_coxnet` object.
#' @param summarize For `collect_metrics()`: `TRUE` for the mean and standard
#'   error over outer splits, `FALSE` for each split's metrics.
#' @exportS3Method tune::collect_metrics
collect_metrics.nested_cv_coxnet <- function(x, ..., summarize = TRUE) {
  per_split <- purrr::list_rbind(Map(function(m, id) {
    m$id <- id
    m
  }, x$.metrics, x$id))
  per_split <- dplyr::select(per_split, "id", ".metric", ".estimator", ".eval_time", ".estimate")
  if (!isTRUE(summarize)) {
    return(per_split)
  }
  summarize_fold_metrics(per_split, character())
}
