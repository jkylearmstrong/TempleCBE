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
#' **Supplied resamples.** With `resamples`, every split is checked before
#' anything is scored: if a subject (or, when `group` is set, a group) is in both
#' the analysis and the assessment set of a split, `cv_coxnet()` stops, because
#' the model would be scored on subjects it was fit on. That is what a row-level
#' [rsample::vfold_cv()] does to start/stop data. Group the folds by the subject
#' instead, with [rsample::group_vfold_cv()] or [rsample::group_bootstraps()];
#' the out-of-bag assessment set of a bootstrap never overlaps its analysis set.
#' `check_subject_overlap = FALSE` switches the check off, for overlap that is
#' deliberate.
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
#' analysis set ([graf_weights()]), and passed to `metrics`. An assessment-set
#' subject with a missing predictor after preprocessing, such as a factor level
#' the analysis set never saw, cannot be predicted: it is left out at every
#' penalty, so all penalties are scored on the same subjects, with one warning
#' per resample that gives the count. A resample with no subject left is skipped.
#' Recurrent events (an event before a subject's last interval) stop with an
#' error unless `recurrent` says otherwise: `"first"` scores the time to first
#' event ([surv_subject_truth()]), and `"gap"` scores the whole recurrent
#' process by gap (see [surv_gap_truth()]). Gap-time concordance is Harrell's
#' C over gaps, using each interval's linear predictor as its risk: it treats
#' a subject's gaps as independent observations and needs no censoring
#' weights. The gap-time Brier score (integrated, `brier_survival_integrated`,
#' or at one time, `brier_survival`) scores each gap by its conditional
#' gap-time survival under the fitted Andersen-Gill model -- the Breslow
#' baseline hazard integrated over the gap's own window, times the gap's
#' linear predictor -- with gap-level inverse-probability-of-censoring
#' weights that condition on the gap's entry time (see [predict_gap_survival()]
#' and [gap_graf_weights()]). Gaps are event-to-event: a subject's rows up to
#' and including an event form one gap, and the baseline hazard is integrated
#' piecewise over the gap's rows, so splitting a gap's rows does not change
#' the score. The censoring distribution `G` is the reverse
#' Kaplan-Meier on the training subjects' full follow-up -- each subject's
#' last interval ([surv_subject_truth()] with `recurrent = "last"`), so a
#' subject's censoring time counts whenever follow-up ends in censoring,
#' not only when it ends at the first event. The Brier score is integrated over `eval_time`
#' the way [yardstick::brier_survival_integrated()] integrates it, and
#' averaged over gaps, so like concordance it describes gaps, not subjects.
#' A gap entering late can run past the last training event time, where the
#' Breslow baseline is flat, so its predicted survival is too high over the
#' rest of the window; the effect is bounded by the 90th-percentile default
#' `eval_time` and the capped censoring weights, and the simulation shows no
#' bias.
#' Gap mode defaults to concordance only; pass `metrics` to add the Brier
#' scores, and `roc_auc_survival` is not available there.
#'
#' **Selection.** `lambda.min` is the penalty, across all `mixture` values,
#' with the best mean `metric` over folds (for time-specific metrics, at the
#' first `eval_time`). `lambda.1se` is the largest penalty, at the same
#' mixture, whose mean is within one standard error of that best mean.
#'
#' **Racing.** With `tune_method = "race_anova"`, the candidates (every
#' `mixture` and penalty) are raced over the resamples the way
#' [finetune::tune_race_anova()] races a tuning grid, but on glmnet directly,
#' so it works for start/stop data and in gap mode, which finetune cannot
#' score. The first `burn_in` resamples score every candidate. Before each
#' later resample, the selection `metric` is modelled as
#' `.estimate ~ candidate + resample` and a candidate is dropped when the
#' `1 - alpha` confidence interval of its difference from the current best is
#' entirely on the worse side; the next resamples score only the survivors, and
#' a mixture with no surviving penalty is no longer fit. Settings are chosen
#' among the survivors, so `lambda.1se` is the largest surviving penalty within
#' one standard error. `metrics` and `fold_metrics` keep the eliminated
#' candidates, with their smaller `n`; `race` lists every candidate with the
#' number of resamples it was scored on and whether it was eliminated. Racing
#' runs the resamples after the burn-in one at a time and cannot be
#' checkpointed.
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
#'   Overrides `v` and `group`. No subject (or group) may be in both the
#'   analysis and the assessment set of a split; see Details and
#'   `check_subject_overlap`.
#' @param v Number of folds when `resamples` is `NULL`.
#' @param metrics A [yardstick::metric_set()] of survival metrics. Defaults to
#'   the integrated Brier score, concordance, time-specific Brier score, and
#'   time-dependent ROC AUC. In gap mode only `concordance_survival`,
#'   `brier_survival_integrated` and `brier_survival` are available.
#' @param eval_time Evaluation times. Defaults to deciles (10th to 90th
#'   percentile) of the observed event times.
#' @param metric Name of the metric used to choose settings; the first metric
#'   in `metrics` by default.
#' @param covariates `"path"` or `"baseline"`; see Details.
#' @param recurrent How recurrent (start/stop) data are scored: `"error"`
#'   (default; a subject with an event before its last interval stops the fit),
#'   `"first"` (score the time to the first event only), or `"gap"` (score the
#'   whole recurrent process by gap; see Details). In gap mode `metrics` may
#'   contain `concordance_survival` (the default there), `brier_survival_integrated`
#'   and `brier_survival`, and `eval_time` sets the gap times the Brier score
#'   is integrated over (with concordance only, it is used only for the refit
#'   model's predictions, not for scoring).
#' @param trunc Lower bound for the censoring probability in the Graf weights.
#' @param parallel If `TRUE`, fit folds with [furrr::future_map()]; set a
#'   [future::plan()] first.
#' @param parallel_over How to parallelize when `parallel` is `TRUE`:
#'   `"resamples"` (default) or `"everything"`, as in [glmnet_IBS()].
#' @param check_subject_overlap If `TRUE` (the default), stop when a supplied
#'   `resamples` puts the same subject (or, when `group` is set, the same group)
#'   in both the analysis and the assessment set of a split; see Details. `FALSE`
#'   skips the check. Folds that `cv_coxnet()` builds itself are never checked,
#'   as they are grouped by subject already.
#' @param checkpoint_dir Optional directory path where per-resample `.rds`
#'   checkpoint files are stored. If `NULL` (default), executes without caching.
#' @param shard Optional integer vector `c(i, n)` specifying that this execution
#'   evaluates shard `i` of `n` resample folds.
#' @param checkpoint_mismatch Action when a checkpoint fingerprint mismatches:
#'   `"recompute"` (default), `"overwrite"`, or `"error"`.
#' @param tune_method `"grid"` (default) scores every candidate on every
#'   resample; `"race_anova"` races them; see Details.
#' @param control Racing settings, used when `tune_method = "race_anova"`: a
#'   list or a [finetune::control_race()] object, of which `burn_in` (default
#'   `3`, at most the number of resamples minus one), `alpha` (default `0.05`)
#'   and `randomize` (default `TRUE`, shuffle the order of the resamples) are
#'   used.
#' @param seed Optional integer seed for reproducible fold generation when
#'   `resamples` is `NULL`. When `checkpoint_dir` is supplied and `resamples` is
#'   `NULL`, defaults to `1L` to ensure cross-validation fold partitions are
#'   identical across calls, enabling instant checkpoint reuse.
#' @param on_complete Optional callback `function(result)` invoked when
#'   `cv_coxnet()` finishes or errors (defaults to
#'   `getOption("templecbe.on_complete", NULL)`).
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
#'   * `eval_time`, `resamples`, `covariates`, and `recurrent` (how recurrent
#'     data were scored: `"error"`, `"first"`, or `"gap"`);
#'   * `tune_method`, and `race`: with racing, one row per candidate
#'     (`mixture`, `penalty`, `n_resamples`, `eliminated`), else `NULL`.
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
                                 recurrent = c("error", "first", "gap"),
                                 parallel = FALSE, parallel_over = c("resamples", "everything"),
                                 check_subject_overlap = TRUE,
                                 checkpoint_dir = NULL, shard = NULL,
                                 checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                 seed = NULL, tune_method = c("grid", "race_anova"), control = NULL,
                                 on_complete = getOption("templecbe.on_complete", NULL), ...) {
  run_with_on_complete("cv_coxnet", on_complete, meta = list(v = v, mixture = mixture), expr = {
    cv_coxnet_impl(
      spec_xy(x, y, subject_id, group),
      mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
      eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
      recurrent = recurrent,
      parallel = parallel, parallel_over = parallel_over,
      check_subject_overlap = check_subject_overlap,
      checkpoint_dir = checkpoint_dir, shard = shard, checkpoint_mismatch = checkpoint_mismatch, seed = seed,
      tune_method = tune_method, control = control, ...
    )
  })
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.matrix <- function(x, y, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                             resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                             metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                             recurrent = c("error", "first", "gap"),
                             parallel = FALSE, parallel_over = c("resamples", "everything"),
                             check_subject_overlap = TRUE,
                             checkpoint_dir = NULL, shard = NULL,
                             checkpoint_mismatch = c("recompute", "overwrite", "error"),
                             seed = NULL, tune_method = c("grid", "race_anova"), control = NULL,
                             on_complete = getOption("templecbe.on_complete", NULL), ...) {
  run_with_on_complete("cv_coxnet", on_complete, meta = list(v = v, mixture = mixture), expr = {
    cv_coxnet_impl(
      spec_xy(matrix_to_df(x), y, subject_id, group),
      mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
      eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
      recurrent = recurrent,
      parallel = parallel, parallel_over = parallel_over,
      check_subject_overlap = check_subject_overlap,
      checkpoint_dir = checkpoint_dir, shard = shard, checkpoint_mismatch = checkpoint_mismatch, seed = seed,
      tune_method = tune_method, control = control, ...
    )
  })
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.formula <- function(formula, data, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                              resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                              metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                              recurrent = c("error", "first", "gap"),
                              parallel = FALSE, parallel_over = c("resamples", "everything"),
                             check_subject_overlap = TRUE,
                              checkpoint_dir = NULL, shard = NULL,
                              checkpoint_mismatch = c("recompute", "overwrite", "error"),
                              seed = NULL, tune_method = c("grid", "race_anova"), control = NULL,
                              on_complete = getOption("templecbe.on_complete", NULL), ...) {
  run_with_on_complete("cv_coxnet", on_complete, meta = list(v = v, mixture = mixture), expr = {
    cv_coxnet_impl(
      spec_formula(formula, data, subject_id, group),
      mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
      eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
      recurrent = recurrent,
      parallel = parallel, parallel_over = parallel_over,
      check_subject_overlap = check_subject_overlap,
      checkpoint_dir = checkpoint_dir, shard = shard, checkpoint_mismatch = checkpoint_mismatch, seed = seed,
      tune_method = tune_method, control = control, ...
    )
  })
}

#' @rdname cv_coxnet
#' @export
cv_coxnet.recipe <- function(x, data, subject_id = NULL, group = NULL, mixture = 1, penalty = NULL,
                             resamples = NULL, v = 10, metrics = NULL, eval_time = NULL,
                             metric = NULL, covariates = c("path", "baseline"), trunc = 0.05,
                             recurrent = c("error", "first", "gap"),
                             parallel = FALSE, parallel_over = c("resamples", "everything"),
                             check_subject_overlap = TRUE,
                             checkpoint_dir = NULL, shard = NULL,
                             checkpoint_mismatch = c("recompute", "overwrite", "error"),
                              seed = NULL, tune_method = c("grid", "race_anova"), control = NULL,
                             on_complete = getOption("templecbe.on_complete", NULL), ...) {
  run_with_on_complete("cv_coxnet", on_complete, meta = list(v = v, mixture = mixture), expr = {
    cv_coxnet_impl(
      spec_recipe(x, data, subject_id, group),
      mixture = mixture, penalty = penalty, resamples = resamples, v = v, metrics = metrics,
      eval_time = eval_time, metric = metric, covariates = covariates, trunc = trunc,
      recurrent = recurrent,
      parallel = parallel, parallel_over = parallel_over,
      check_subject_overlap = check_subject_overlap,
      checkpoint_dir = checkpoint_dir, shard = shard, checkpoint_mismatch = checkpoint_mismatch, seed = seed,
      tune_method = tune_method, control = control, ...
    )
  })
}

#' Use a `cv_coxnet` Result
#'
#' Predict from, tidy, plot, or collect the metrics of a [cv_coxnet()] result.
#' Predictions and coefficients come from the model refit to all the data at
#' the chosen `mixture`.
#'
#' @param object,x A [cv_coxnet()] result.
#' @param new_data A data frame of new predictors.
#' @param type `"linear_pred"` (the default), `"survival"`, or `"time"` (restricted mean survival time),
#'   as for [predict.coxnet_model()].
#' @param eval_time,increasing As for [predict.coxnet_model()].
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

#' @rdname templecbe_methods
#' @export
print.cv_coxnet <- function(x, ...) {
  folds <- if (!is.null(x$resamples)) nrow(x$resamples) else length(unique(x$fold_metrics$id))
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
                           covariates = c("path", "baseline"), trunc = 0.05,
                           recurrent = c("error", "first", "gap"), parallel = FALSE,
                           parallel_over = c("resamples", "everything"),
                           check_subject_overlap = TRUE,
                           checkpoint_dir = NULL,
                           shard = NULL,
                           checkpoint_mismatch = c("recompute", "overwrite", "error"),
                           seed = NULL, tune_method = c("grid", "race_anova"), control = NULL, ...) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  tune_method <- match.arg(tune_method)
  race <- if (tune_method == "race_anova") race_coxnet_control(control) else NULL
  if (!is.null(race) && !is.null(checkpoint_dir)) {
    stop(
      "`tune_method = \"race_anova\"` does not checkpoint: which candidates a resample scores ",
      "depends on the resamples scored before it. Use `tune_method = \"grid\"` with `checkpoint_dir`.",
      call. = FALSE
    )
  }
  rlang::check_installed(
    c("glmnet", "survival", "rsample", "yardstick", "withr"),
    reason = "to cross-validate `coxnet()` models."
  )
  covariates <- match.arg(covariates)
  recurrent <- match.arg(recurrent)
  gap_mode <- identical(recurrent, "gap")
  # Gap mode scores gap-time concordance and gap-time Brier scores; the other
  # yardstick survival metrics need one event time per subject and a survival
  # curve per subject, which gaps do not have.
  gap_brier_names <- c("brier_survival_integrated", "brier_survival")
  if (gap_mode) {
    if (is.null(metrics)) {
      metrics <- yardstick::metric_set(yardstick::concordance_survival)
    } else {
      gap_info <- surv_metric_info(metrics)
      if (!all(gap_info$metric %in% c("concordance_survival", gap_brier_names))) {
        stop(
          "`recurrent = \"gap\"` scores gap-time concordance and gap-time Brier scores only; pass ",
          "`metrics` with `concordance_survival`, `brier_survival_integrated` or `brier_survival`. ",
          "`roc_auc_survival` is not available in gap mode.",
          call. = FALSE
        )
      }
    }
  }
  check_true_false(check_subject_overlap, "check_subject_overlap")
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
  truth_full <- if (gap_mode) {
    surv_gap_truth(full$y, subject_keys(full$y, subject_values(data, spec)))
  } else {
    surv_subject_truth(full$y, subject_keys(full$y, subject_values(data, spec)), recurrent = recurrent)
  }
  if (!gap_mode && any(truth_full$.entry > 0)) {
    warning(
      "Some subjects enter after time 0. Survival is predicted from time 0 and the Graf ",
      "censoring weights don't correct for delayed entry.",
      call. = FALSE
    )
  }
  if (is.null(eval_time)) {
    eval_time <- if (gap_mode) {
      default_gap_eval_time(truth_full)
    } else {
      default_eval_time(truth_full$.truth)
    }
  }
  needs_two <- any(info$class == "integrated_survival_metric")
  check_eval_time(eval_time, min_length = if (needs_two) 2 else 1)

  paths <- lapply(mixture, function(m) {
    path_user %||% fit_glmnet_cox(full$x, full$y, m, NULL, glmnet_args)$lambda
  })

  user_resamples <- resamples
  if (is.null(user_resamples)) {
    fold_seed <- seed
    if (is.null(fold_seed) && !is.null(checkpoint_dir)) {
      fold_seed <- 1L
      message("cv_coxnet(): Defaulting to seed = 1L for cross-validation fold generation to ensure checkpoints can be reused across calls. Set `seed` to change.")
    }
    resamples <- if (!is.null(fold_seed)) {
      withr::with_seed(fold_seed, make_coxnet_folds(data, spec, v))
    } else {
      make_coxnet_folds(data, spec, v)
    }
  }
  if (!inherits(resamples, "rset")) {
    stop("`resamples` must be an rsample rset, such as rsample::group_vfold_cv().", call. = FALSE)
  }
  if (nrow(resamples$splits[[1]]$data) != nrow(data)) {
    stop("`resamples` must be built on `data`.", call. = FALSE)
  }
  # Folds made above are grouped by subject already; only supplied ones can leak.
  if (check_subject_overlap && !is.null(user_resamples)) {
    check_resample_overlap(resamples, spec, "`resamples`")
  }

  # In gap mode the requested metrics split into the concordance path and the
  # Brier path (which needs the training baseline and censoring distribution).
  gap_has_concordance <- gap_mode && any(info$metric == "concordance_survival")
  gap_brier_metrics <- if (gap_mode) {
    brier_names <- intersect(info$metric, gap_brier_names)
    if (length(brier_names)) yardstick_metric_set(brier_names) else NULL
  } else {
    NULL
  }
  gap_brier_info <- if (!is.null(gap_brier_metrics)) surv_metric_info(gap_brier_metrics) else NULL

  # `active` holds, for each `mixture`, the penalties still to be scored: the
  # whole path in a grid search, the surviving candidates while racing (an
  # empty vector skips that mixture's fit).
  score_split <- function(split, id, active = paths) {
    analysis <- rsample::analysis(split)
    assessment <- rsample::assessment(split)
    train <- spec$fit_frame(analysis)
    test <- spec$new_frame(assessment, train$blueprint)
    # Keys, not raw identifiers: a bootstrap analysis set repeats subjects.
    subject_test <- subject_keys(test$y, subject_values(assessment, spec))
    # Subjects whose predictors are missing are dropped before the truth and the
    # weights are built, so every penalty is scored on the same subjects.
    usable <- complete_assessment_rows(test$x, subject_test, paste0("resample `", id, "`"))
    if (!any(usable)) {
      return(NULL)
    }
    test <- list(x = test$x[usable, , drop = FALSE], y = test$y[usable])
    subject_test <- subject_test[usable]
    if (gap_mode) {
      # Gap-time concordance needs no censoring weights and no baseline
      # hazard: each assessment interval is scored by its linear predictor.
      # The gap-time Brier score needs both, from the training data.
      return(purrr::list_rbind(purrr::pmap(list(mixture, paths, active), function(m, path, score_at) {
        if (!length(score_at)) return(NULL)
        fit <- tryCatch(fit_glmnet_cox(train$x, train$y, m, path, glmnet_args), error = function(e) e)
        if (inherits(fit, "error")) {
          warning("glmnet failed on a resample at mixture ", m, ": ", conditionMessage(fit), call. = FALSE)
          return(NULL)
        }
        parts <- list()
        if (gap_has_concordance) {
          parts <- c(parts, list(score_gap_path(fit, test$x, test$y, subject_test, penalty = score_at)))
        }
        if (!is.null(gap_brier_metrics)) {
          subject_train <- subject_keys(train$y, subject_values(analysis, spec))
          parts <- c(parts, list(score_gap_brier_path(
            fit, train$x, train$y, subject_train, test$x, test$y, subject_test,
            penalty = score_at, eval_time = eval_time, metrics = gap_brier_metrics,
            info = gap_brier_info, trunc = trunc
          )))
        }
        res <- purrr::list_rbind(parts)
        res$mixture <- m
        res
      })))
    }
    truth_train <- surv_subject_truth(train$y, subject_keys(train$y, subject_values(analysis, spec)), recurrent = recurrent)
    truth_test <- surv_subject_truth(test$y, subject_test, recurrent = recurrent)
    weights <- graf_weights(truth_test$.truth, eval_time, censoring_km(truth_train$.truth), trunc)

    purrr::list_rbind(purrr::pmap(list(mixture, paths, active), function(m, path, score_at) {
      if (!length(score_at)) return(NULL)
      fit <- tryCatch(fit_glmnet_cox(train$x, train$y, m, path, glmnet_args), error = function(e) e)
      if (inherits(fit, "error")) {
        warning("glmnet failed on a resample at mixture ", m, ": ", conditionMessage(fit), call. = FALSE)
        return(NULL)
      }
      res <- score_coxnet_path(
        fit, train$x, train$y, test$x, test$y, subject_test,
        penalty = score_at, eval_time = eval_time, truth = truth_test, weights = weights,
        metrics = metrics, info = info, covariates = covariates
      )
      res$mixture <- m
      res
    }))
  }

  # The fold function travels to the workers with only what it uses: its
  # enclosing frame holds `data`, the full design matrix and every split, and
  # `serialize()` writes each split's data separately. Each worker receives
  # its own splits as the mapped input instead.
  worker_spec <- spec
  worker_spec$data <- NULL
  score_split <- with_worker_env(
    score_split,
    covariates = covariates, eval_time = eval_time, gap_mode = gap_mode, glmnet_args = glmnet_args,
    gap_brier_info = gap_brier_info, gap_brier_metrics = gap_brier_metrics,
    gap_has_concordance = gap_has_concordance,
    info = info, metrics = metrics, mixture = mixture, paths = paths, recurrent = recurrent,
    spec = worker_spec, trunc = trunc
  )

  is_parallel <- isTRUE(parallel) ||
    (is.character(parallel) && parallel %in% c("resamples", "everything", "true")) ||
    (is.list(parallel) && isTRUE(parallel$parallel_over %in% c("resamples", "everything"))) ||
    (!missing(parallel_over) && isTRUE(match.arg(parallel_over) %in% c("resamples", "everything")) && !identical(parallel, FALSE))

  fold_ids <- resample_ids(resamples)
  n_splits <- length(resamples$splits)
  partial_shard <- NULL

  if (!is.null(checkpoint_dir)) {
    if (!dir.exists(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    }
    ckpt_schema <- checkpoint_schema_version()
    metrics_repr <- if (!is.null(metrics)) {
      m_attr <- attr(metrics, "metrics", exact = TRUE)
      if (!is.null(m_attr)) {
        list(metric_names = sort(names(m_attr)))
      } else {
        paste0(as.character(openssl::sha256(serialize(metrics, NULL))), collapse = "")
      }
    } else {
      NULL
    }

    split_fps <- vapply(seq_len(n_splits), function(i) {
      sp <- resamples$splits[[i]]
      data_repr <- if (inherits(sp, "rsplit") && is.data.frame(sp$data)) {
        paste0(as.character(openssl::sha256(serialize(sp$data, NULL))), collapse = "")
      } else {
        NULL
      }
      payload <- list(
        schema = ckpt_schema,
        seed_scheme = current_seed_scheme(),
        seed = seed,
        data = data_repr,
        split_in = sp$in_id,
        mixture = mixture,
        paths = paths,
        eval_time = eval_time,
        metrics = metrics_repr,
        covariates = covariates,
        recurrent = recurrent,
        trunc = trunc,
        # The predictor set is not in the data hash (the same data frame
        # feeds `~ x1 + x2` and `~ x1 + x3`), so the canonicalised
        # preprocessor belongs in the fingerprint (R7-02). The `paths`
        # entry is no proxy for it: glmnet's path is a fixed geometric
        # sequence, bit-identical for either predictor set.
        preprocessor = canonicalize_preprocessor_for_hash(spec$preprocessor),
        # Every glmnet `...` argument changes the fits (R6-01), so it
        # belongs in the fingerprint. It is always present, so checkpoints
        # written before this fix mismatch once and are recomputed: they
        # carry no record of the arguments they were computed with.
        glmnet_args = glmnet_args
      )
      # The `templecbe.cox_ties` option is the fit-time default when no
      # explicit `cox.ties` is passed (see fit_glmnet_cox()). With an
      # explicit penalty path the fitted path does not move with it, so a
      # flipped option would otherwise reuse the old checkpoints too. The
      # default leaves the fingerprint as before, so existing breslow
      # checkpoints stay valid.
      if (!identical(get_templecbe_cox_ties(), "breslow")) payload$cox_ties <- get_templecbe_cox_ties()
      paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
    }, character(1))

    run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, ids = fold_ids), NULL))), collapse = "")

    shard_parsed <- validate_shard_spec(shard)
    active_mask <- if (!is.null(shard_parsed)) {
      ((seq_len(n_splits) - 1L) %% shard_parsed[2]) + 1L == shard_parsed[1]
    } else {
      rep(TRUE, n_splits)
    }
    if (!is.null(shard_parsed) && any(!active_mask)) {
      partial_shard <- list(
        shard = shard_parsed, n_splits = n_splits,
        evaluated_folds = fold_ids[active_mask], missing_folds = fold_ids[!active_mask]
      )
    }

    # Verify existing checkpoints up front
    mismatch_count <- 0L
    for (i in seq_len(n_splits)) {
      sp_id <- fold_ids[i]
      safe_id <- gsub("[^A-Za-z0-9._-]", "_", sp_id)
      cf <- file.path(checkpoint_dir, sprintf("cv_coxnet_%s.rds", safe_id))
      if (file.exists(cf)) {
        chk <- read_verified_checkpoint(cf, split_fps[i], checkpoint_mismatch = checkpoint_mismatch)
        if (identical(chk$status, "mismatch")) {
          mismatch_count <- mismatch_count + 1L
          unlink(cf)
        }
      }
    }
    if (mismatch_count > 0L) {
      message(sprintf(
        "cv_coxnet(): Discarded %d cached task(s) in '%s' due to mismatched fingerprint. Recomputing...",
        mismatch_count, checkpoint_dir
      ))
    }

    eval_split_ckpt <- function(split, i) {
      sp_id <- fold_ids[i]
      safe_id <- gsub("[^A-Za-z0-9._-]", "_", sp_id)
      cf <- file.path(checkpoint_dir, sprintf("cv_coxnet_%s.rds", safe_id))
      exp_fp <- split_fps[i]

      if (file.exists(cf)) {
        chk <- read_verified_checkpoint(cf, exp_fp, checkpoint_mismatch = "recompute")
        if (identical(chk$status, "hit")) {
          return(chk$value)
        }
      }
      if (!active_mask[i]) {
        return(NULL)
      }

      val <- score_split(split, sp_id)
      if (!is.null(val)) {
        write_verified_checkpoint(
          val, cf, fingerprint = exp_fp, run_fingerprint = run_fp,
          meta = list(fn = "cv_coxnet", fold_id = sp_id, index = i)
        )
      }
      val
    }

    eval_split_ckpt <- with_worker_env(
      eval_split_ckpt,
      active_mask = active_mask, checkpoint_dir = checkpoint_dir, fold_ids = fold_ids,
      run_fp = run_fp, score_split = score_split, split_fps = split_fps
    )
    results <- if (isTRUE(is_parallel)) {
      rlang::check_installed(c("furrr", "future"), reason = "to fit folds in parallel.")
      furrr::future_map2(resamples$splits, seq_len(n_splits), eval_split_ckpt, .options = furrr::furrr_options(seed = TRUE, packages = furrr_worker_packages()))
    } else {
      purrr::map2(resamples$splits, seq_len(n_splits), eval_split_ckpt)
    }
  } else {
    map_splits <- function(splits, ids) {
      if (isTRUE(is_parallel)) {
        rlang::check_installed(c("furrr", "future"), reason = "to fit folds in parallel.")
        furrr::future_map2(splits, ids, score_split, .options = furrr::furrr_options(seed = TRUE, packages = furrr_worker_packages()))
      } else {
        purrr::map2(splits, ids, score_split)
      }
    }
    if (is.null(race)) {
      results <- map_splits(resamples$splits, fold_ids)
    } else {
      raced <- race_coxnet_resamples(
        resamples$splits, fold_ids, score_split = score_split, map_splits = map_splits,
        mixture = mixture, paths = paths, info = info, metric = metric,
        eval_time = eval_time, control = race
      )
      results <- raced$results
    }
  }

  fold_metrics <- purrr::list_rbind(Map(function(res, id) {
    if (is.null(res) || !nrow(res)) return(NULL)
    res$id <- id
    res
  }, results, fold_ids))
  if (!is.null(partial_shard)) {
    warning(
      sprintf("cv_coxnet(): shard %d/%d evaluated only %d of %d resample(s) (%s); the summary averages over the evaluated folds only. ",
              partial_shard$shard[1], partial_shard$shard[2], length(partial_shard$evaluated_folds),
              partial_shard$n_splits, paste(partial_shard$evaluated_folds, collapse = ", ")),
      "Collect the other shards' checkpoints to complete it; see `x$shard_partial`.",
      call. = FALSE
    )
  }
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
  race_info <- NULL
  # A raced candidate that was eliminated was scored on fewer resamples than
  # the survivors, so, as in finetune, settings are chosen among survivors.
  candidates <- summary
  if (!is.null(race)) {
    race_info <- raced$candidates
    survivors <- race_info[!race_info$eliminated, c("mixture", "penalty"), drop = FALSE]
    candidates <- dplyr::semi_join(summary, survivors, by = c("mixture", "penalty"))
  }
  selection <- select_coxnet_penalty(candidates, metric, direction, metric_eval_time)
  best_by_mixture <- purrr::list_rbind(lapply(unique(candidates$mixture), function(m) {
    s <- tryCatch(
      select_coxnet_penalty(candidates[candidates$mixture == m, ], metric, direction, metric_eval_time),
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
      recurrent = recurrent,
      fit = new_coxnet(final, selection$lambda_min, selection$mixture, full$x, full$y, full$blueprint),
      resamples = resamples,
      shard_partial = partial_shard,
      tune_method = tune_method,
      race = race_info
    ),
    class = "cv_coxnet"
  )
}

#' Default Evaluation Times From Gap Event Times
#'
#' Deciles of the observed whole-gap event times: gaps are event-to-event, so
#' splitting a gap's rows does not change the grid. With Brier metrics these
#' are the gap times the score is integrated over; with concordance only they
#' feed the refit model's predictions, not the scoring. A single distinct
#' event time is enough, where the subject-level default needs two.
#'
#' @param gap_truth Output of [surv_gap_truth()].
#' @keywords internal
#' @noRd
default_gap_eval_time <- function(gap_truth) {
  collapsed <- collapse_gap_truth(gap_truth)$truth
  gap_event_times <- unname(collapsed$.truth[collapsed$.truth[, "status"] == 1, "time"])
  if (!length(gap_event_times)) {
    stop("No gap events to choose default evaluation times from; pass `eval_time`.", call. = FALSE)
  }
  gap_times <- sort(unique(stats::quantile(
    gap_event_times, probs = seq(0.1, 0.9, by = 0.1), type = 1, names = FALSE
  )))
  if (!length(gap_times)) {
    stop("Too few gap event times to choose default evaluation times; pass `eval_time`.", call. = FALSE)
  }
  gap_times
}

#' Score a glmnet Cox Fit Along a Penalty Path by Gap-Time Concordance
#'
#' For recurrent data: each assessment interval is a gap, scored by its
#' linear predictor (see [gap_time_concordance()]). The linear predictors are
#' reordered to the gap-truth row order (by subject, start, stop) before
#' scoring.
#'
#' @return A tibble with `penalty`, `.metric`, `.estimator`, `.eval_time`
#'   (`NA`) and `.estimate`, one row per penalty. A penalty whose linear
#'   predictors are not all finite (glmnet can return `NaN` at an extreme
#'   penalty on a small fold) is scored `NA` instead of stopping the run,
#'   mirroring [compute_surv_metrics()].
#' @keywords internal
#' @noRd
score_gap_path <- function(fit, x_new, y_new, subject_new, penalty) {
  gap_truth <- surv_gap_truth(y_new, subject_new)
  parts <- surv_components(y_new)
  ord <- order(subject_new, parts$start, parts$stop)
  lp <- coxnet_link(fit, x_new, penalty)[ord, , drop = FALSE]
  purrr::list_rbind(lapply(seq_along(penalty), function(l) {
    lp_l <- lp[, l]
    est <- if (!is.numeric(lp_l) || length(lp_l) != nrow(gap_truth) || any(!is.finite(lp_l))) {
      NA_real_
    } else {
      gap_time_concordance(gap_truth, lp_l)
    }
    tibble::tibble(
      penalty = penalty[l],
      .metric = "concordance_survival",
      .estimator = "standard",
      .eval_time = NA_real_,
      .estimate = est
    )
  }))
}

#' Score a glmnet Cox Fit Along a Penalty Path by Gap-Time Brier Score
#'
#' For recurrent data: each assessment gap is scored by its conditional
#' gap-time survival ([predict_gap_survival()]) with gap-level IPCW weights
#' ([gap_graf_weights()]), and the Brier metrics are computed by yardstick on
#' one row per gap. The baseline hazard and the censoring distribution come
#' from the training data, as in [score_coxnet_path()]. Gaps are event-to-event
#' ([collapse_gap_truth()]): a subject's rows up to and including an event form
#' one gap, so a gap split across rows by a time-varying covariate is scored
#' once, with its survival integrated piecewise over its rows -- never as
#' several gaps ending in fake censoring. Concordance keeps the per-row
#' definition ([score_gap_path()]).
#'
#' @return A tibble with `penalty`, `.metric`, `.estimator`, `.eval_time`
#'   and `.estimate`, one row per penalty per Brier metric (per evaluation
#'   time for time-specific metrics). A penalty whose predictions are not all
#'   finite is scored `NA`, mirroring [compute_surv_metrics()].
#' @keywords internal
#' @noRd
score_gap_brier_path <- function(fit, x_train, y_train, subject_train, x_new, y_new, subject_new,
                                 penalty, eval_time, metrics, info, trunc) {
  train <- surv_components(y_train)
  bh <- breslow_cumhaz(coxnet_link(fit, x_train, penalty), train$start, train$stop, train$status)
  censoring <- censoring_km(
    surv_subject_truth(y_train, subject_train, recurrent = "last")$.truth
  )
  gap_truth <- surv_gap_truth(y_new, subject_new)
  parts <- surv_components(y_new)
  ord <- order(subject_new, parts$start, parts$stop)
  lp <- coxnet_link(fit, x_new, penalty)[ord, , drop = FALSE]
  collapsed <- collapse_gap_truth(gap_truth)
  surv <- predict_gap_survival(
    bh, collapsed$truth$.entry, lp, eval_time,
    start = parts$start[ord], stop = parts$stop[ord], group = collapsed$group
  )
  weights <- gap_graf_weights(collapsed$truth, eval_time, censoring, trunc)
  frame <- surv_metric_frame(collapsed$truth$.truth, surv, eval_time, weights, penalty)
  compute_surv_metrics(frame, metrics, info)
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
  # A penalty whose predictions are not finite (glmnet can return NaN
  # survival at an extreme penalty on a small fold) is scored NA instead of
  # stopping yardstick, and so the whole resample, with "No (non-missing)
  # observations". Selection and racing skip NA estimates.
  pens <- unique(frame$penalty)
  bad_pen <- pens[vapply(pens, function(pen) {
    rows <- frame$penalty == pen
    !all(is.finite(frame$.pred_time[rows])) ||
      !all(vapply(frame$.pred[rows], function(p) all(is.finite(p$.pred_survival)), logical(1)))
  }, logical(1))]
  bad_rows <- frame$penalty %in% bad_pen
  scored <- if (any(!bad_rows)) score_surv_metrics(frame[!bad_rows, , drop = FALSE], metrics, info) else NULL
  if (!length(bad_pen)) {
    return(scored)
  }
  eval_time <- frame$.pred[[which(bad_rows)[1]]]$.eval_time
  na_rows <- purrr::list_rbind(lapply(bad_pen, function(pen) {
    na_surv_metric_rows(pen, info, eval_time)
  }))
  out <- dplyr::bind_rows(scored, na_rows)
  out[order(match(out$penalty, pens)), , drop = FALSE]
}

# One NA row per metric (per evaluation time for time-dependent metrics).
na_surv_metric_rows <- function(penalty, info, eval_time) {
  purrr::list_rbind(lapply(seq_len(nrow(info)), function(j) {
    times <- if (info$class[j] == "dynamic_survival_metric") eval_time else NA_real_
    tibble::tibble(
      penalty = penalty, .metric = info$metric[j], .estimator = "standard",
      .eval_time = times, .estimate = NA_real_
    )
  }))
}

score_surv_metrics <- function(frame, metrics, info) {
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

# Which rows of an assessment set can be scored. Preprocessing learned on the
# analysis set can leave an assessment predictor missing, most often a factor
# level the analysis set never saw, which hardhat::forge() turns into NA with
# only a warning. Such a subject has no prediction at the penalties where that
# predictor's coefficient is nonzero but one, from the remaining predictors, at
# the others, so the penalties would be scored on different subjects. The
# subject, with all its rows, is left out at every penalty instead, and one
# warning says how many were.
complete_assessment_rows <- function(x, subject, where) {
  complete <- stats::complete.cases(x)
  if (all(complete)) {
    return(rep(TRUE, length(complete)))
  }
  dropped <- unique(subject[!complete])
  keep <- !subject %in% dropped
  warning(
    length(dropped), " of ", length(unique(subject)), " subject(s) in ", where, " have missing predictor values ",
    "after preprocessing, for example a factor level not seen in the analysis set, and are left out of the ",
    "scores at every penalty",
    if (!any(keep)) ", which leaves none to score",
    ".",
    call. = FALSE
  )
  keep
}

check_true_false <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

# Stops when a split of `resamples` (an rset, or the outer splits of a nested_cv
# object) puts the same subject, or with `group` the same group, in both its
# analysis and its assessment set: the model would be scored on subjects it was
# fit on, as row-level folds do to a subject's start/stop rows. Works on row
# indices, so no analysis or assessment data frame is built. The out-of-bag
# assessment set of a bootstrap is disjoint from its analysis set and passes.
check_resample_overlap <- function(resamples, spec, what) {
  splits <- resamples$splits
  data <- splits[[1]]$data
  key_col <- spec$group_col
  if (!is.null(key_col) && !key_col %in% names(data)) {
    return(invisible(TRUE))
  }
  keys <- if (is.null(key_col)) seq_len(nrow(data)) else data[[key_col]]
  unit <- if (is.null(key_col) || identical(key_col, spec$subject_col)) "subject" else "group"
  ids <- resample_ids(resamples)
  for (i in seq_along(splits)) {
    shared <- intersect(
      unique(keys[as.integer(splits[[i]], data = "analysis")]),
      unique(keys[as.integer(splits[[i]], data = "assessment")])
    )
    if (length(shared)) {
      stop(
        what, " put ", length(shared), " ", unit, if (length(shared) > 1) "s", " in both the analysis and ",
        "the assessment set of resample `", ids[i], "` (", format_ids(shared, 5), "), so ",
        if (length(shared) > 1) "they" else "it", " would be scored on data the model was fit on. ",
        "Keep every ", unit, " on one side, for example with rsample::group_vfold_cv() grouped by the ",
        if (unit == "group") "`group`" else "`subject_id`", " column; row-level folds such as ",
        "rsample::vfold_cv() split a subject's start/stop rows. Set `check_subject_overlap = FALSE` ",
        "if the overlap is deliberate.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
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
    # The predictor names are the `x`/`y` spec's preprocessor: the data
    # hash alone cannot tell two column sets apart when the data frame
    # carries extra columns, so the fingerprint hashes these (R7-02).
    preprocessor = pred_cols,
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
  check_formula_specials(formula)
  check_id_columns(data, subject_id, group)
  drop <- unique(c(subject_id, group))
  model_data <- function(d) d[setdiff(names(d), drop)]
  spec_without_frame_data(list(
    data = data,
    subject_col = subject_id,
    group_col = group %||% subject_id,
    # Kept for the checkpoint fingerprint: the data hash alone cannot tell
    # `~ x1 + x2` from `~ x1 + x3` on the same data frame (R7-02).
    preprocessor = formula,
    fit_frame = function(d) {
      m <- hardhat::mold(formula, model_data(d), blueprint = coxnet_formula_blueprint())
      list(x = predictors_matrix(m$predictors), y = surv_outcome(m$outcomes), blueprint = m$blueprint)
    },
    new_frame = function(d, blueprint) {
      f <- hardhat::forge(model_data(d), blueprint, outcomes = TRUE)
      list(x = predictors_matrix(f$predictors), y = surv_outcome(f$outcomes))
    }
  ))
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
  spec_without_frame_data(list(
    data = data,
    subject_col = subject_id,
    group_col = group %||% subject_id,
    # Kept for the checkpoint fingerprint: the data hash alone cannot tell
    # two recipes apart on the same data frame (R7-02).
    preprocessor = recipe,
    fit_frame = function(d) {
      m <- hardhat::mold(recipe, d)
      check_not_predictor(m$predictors)
      list(x = predictors_matrix(m$predictors), y = surv_outcome(m$outcomes), blueprint = m$blueprint)
    },
    new_frame = function(d, blueprint) {
      f <- hardhat::forge(d, blueprint, outcomes = TRUE)
      list(x = predictors_matrix(f$predictors), y = surv_outcome(f$outcomes))
    }
  ))
}

#' Drop `data` From the Frame a Coxnet Spec's Closures Enclose
#'
#' `fit_frame()` and `new_frame()` are defined in the frame of
#' `spec_formula()` / `spec_recipe()`, whose `data` argument they do not use.
#' Without this, a spec sent to a parallel worker (or saved) would carry that
#' data inside its closures, besides the copy in `spec$data`.
#'
#' @param spec The spec list, built in the caller's frame.
#' @return `spec`.
#' @keywords internal
#' @noRd
spec_without_frame_data <- function(spec) {
  force(spec)
  frame <- parent.frame()
  if (exists("data", envir = frame, inherits = FALSE)) {
    rm("data", envir = frame)
  }
  spec
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
