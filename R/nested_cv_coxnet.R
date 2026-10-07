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
#' rows stay together. This is checked: if a subject (or, when `group` is set,
#' a group) is in both the analysis and the assessment set of an outer split or
#' of any inner resample, `nested_cv_coxnet()` stops before fitting anything,
#' because the model would be scored on subjects it was fit on. Out-of-bag
#' assessment sets of bootstraps pass. `check_subject_overlap = FALSE` skips the
#' check, for overlap that is deliberate.
#'
#' The outer assessment set is scored as in [cv_coxnet()]: a subject with a
#' missing predictor after preprocessing, such as a factor level the outer
#' analysis set never saw, is left out with a warning, and an outer split with
#' no subject left is an error.
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
#'   [cbe_loco_mp_coxnet()] is run on each outer analysis set at the `mixture`
#'   and penalty chosen there, with the same glmnet arguments (see `...`). It
#'   draws its minipatches by subject, so `group` does not apply to it, and a
#'   warning says so when `group` is set.
#' @param ... Further arguments passed to [glmnet::glmnet()], such as `cox.ties`,
#'   `standardize`, `penalty.factor`, or `nlambda`. With `importance = "loco_mp"`
#'   they are passed to [cbe_loco_mp_coxnet()] as well, so the minipatch models
#'   are fit like the tuned ones; arguments that [cbe_loco_mp_coxnet()] sets
#'   itself, such as `B`, are not forwarded.
#' @param check_subject_overlap If `TRUE` (the default), stop when an outer
#'   split, or an inner resample, puts the same subject (or, when `group` is
#'   set, the same group) in both its analysis and its assessment set; see
#'   Details and [cv_coxnet()]. `FALSE` skips the check.
#' @param tune_method Method for tuning hyperparameters across the inner resamples:
#'   `"grid"` (default, standard [cv_coxnet()] grid search), `"race_anova"`
#'   ([finetune::tune_race_anova()] racing), or `"race_win_loss"`
#'   ([finetune::tune_race_win_loss()] racing; needs BradleyTerry2).
#' @inheritParams cv_coxnet
#' @return A tibble, of class `nested_cv_coxnet`, with one row per outer split:
#'   `id`, the chosen `mixture` and `penalty`, `.metrics` (outer assessment-set
#'   metrics), `.coefs` (coefficients of the refit), and `.inner` (the inner
#'   cross-validation's summarized metrics). If `importance = "loco_mp"`, includes
#'   `.importance` with tidy LOCO-MP statistical inference. `tune::collect_metrics()`
#'   averages `.metrics` over outer splits.
#' @seealso [cv_coxnet()], [coxnet()], [cbe_loco_mp_coxnet()], [tune_race_survival()]
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
                             trunc = 0.05, parallel = FALSE, importance = c("none", "loco_mp"),
                             check_subject_overlap = TRUE,
                              tune_method = c("grid", "race_anova", "race_win_loss"), ...) {
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
  tune_method <- match.arg(tune_method)
  metrics <- metrics %||% default_surv_metrics()
  info <- surv_metric_info(metrics)

  full_data <- object$splits[[1]]$data
  spec <- make_coxnet_spec(preprocessor, full_data, subject_id, group)
  if (tune_method != "grid") {
    check_race_outcome_type(surv_components(spec$fit_frame(full_data)$y)$type, tune_method)
  }
  # The outer splits and every inner resample are user-supplied, so all of them
  # are checked here, before any model is fit; the inner cross-validations below
  # then skip their own check.
  check_true_false(check_subject_overlap, "check_subject_overlap")
  if (check_subject_overlap) {
    check_resample_overlap(object, spec, "The outer splits of `object`")
    for (i in seq_along(object$splits)) {
      check_resample_overlap(
        object$inner_resamples[[i]], spec,
        paste0("The inner resamples of outer split `", object$id[i], "`")
      )
    }
  }
  # The minipatches of LOCO-MP are fit with the same glmnet arguments as the
  # tuned model (`standardize`, `cox.ties`, `penalty.factor`, ...), so the
  # importance describes the model that was assessed.
  loco_glmnet_args <- if (importance == "loco_mp") forwardable_glmnet_args(list(...)) else list()
  if (importance == "loco_mp" && !is.null(group) && !identical(group, subject_id)) {
    warning(
      "`group` does not apply to the LOCO-MP importance: minipatches are drawn by subject, so subjects of ",
      "one group can fall both in and out of a minipatch.",
      call. = FALSE
    )
  }
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

    if (tune_method %in% c("race_anova", "race_win_loss")) {
      rlang::check_installed(c("finetune", "parsnip", "workflows"), reason = "for racing in nested_cv_coxnet.")
      mix_param <- if (length(mixture) == 1) mixture else tune::tune()
      spec_cox <- parsnip::set_engine(
        parsnip::proportional_hazards(penalty = tune::tune(), mixture = mix_param),
        "coxnet", ...
      ) |> parsnip::set_mode("censored regression")
      wflow <- workflows::workflow() |>
        workflows::add_model(spec_cox)
      wflow <- if (inherits(preprocessor, "recipe")) {
        workflows::add_recipe(wflow, preprocessor)
      } else {
        workflows::add_formula(wflow, preprocessor)
      }
      race_res <- tune_race_survival(
        wflow,
        resamples = object$inner_resamples[[i]],
        fn = if (tune_method == "race_anova") "tune_race_anova" else "tune_race_win_loss",
        grid = if (is.numeric(penalty)) length(penalty) else 20,
        metrics = metrics,
        eval_time = eval_time
      )
      best <- tune::select_best(race_res, metric = metric %||% "brier_survival_integrated")
      chosen_mix <- if ("mixture" %in% names(best)) best$mixture else mixture[1]
      chosen <- best$penalty

      refit <- coxnet(
        preprocessor,
        data = analysis,
        mixture = chosen_mix,
        penalty = chosen,
        ...
      )
      inner_metrics <- tune::collect_metrics(race_res)
    } else {
      inner <- cv_coxnet_impl(
        spec, data = analysis, mixture = mixture, penalty = penalty,
        resamples = object$inner_resamples[[i]], metrics = metrics, eval_time = eval_time,
        metric = metric, covariates = covariates, trunc = trunc, parallel = FALSE,
        check_subject_overlap = FALSE, ...
      )
      chosen_mix <- inner$mixture
      chosen <- if (rule == "min") inner$lambda_min else inner$lambda_1se
      refit <- inner$fit
      inner_metrics <- inner$metrics
    }

    test <- spec$new_frame(assessment, refit$blueprint)
    subject_test <- subject_keys(test$y, subject_values(assessment, spec))
    # As in cv_coxnet(): a subject with missing predictors is left out, with a warning.
    usable <- complete_assessment_rows(
      test$x, subject_test, paste0("the outer assessment set of split `", object$id[i], "`")
    )
    if (!any(usable)) {
      stop("Outer split `", object$id[i], "` has no assessment subject to score.", call. = FALSE)
    }
    test <- list(x = test$x[usable, , drop = FALSE], y = test$y[usable])
    subject_test <- subject_test[usable]
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
        do.call(cbe_loco_mp_coxnet, c(
          list(
            formula = preprocessor, data = analysis, subject_id = subject_id,
            mixture = chosen_mix, penalty = chosen, eval_time = eval_time,
            B = 30, trunc = trunc, covariates = covariates, parallel = FALSE
          ),
          loco_glmnet_args
        ))
      }, error = function(e) {
        warning(
          "LOCO-MP importance failed for outer split ", object$id[i], ": ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      })
      if (!is.null(loco_fit)) list(generics::tidy(loco_fit)) else list(NULL)
    } else {
      NULL
    }

    res_row <- tibble::tibble(
      id = object$id[i],
      mixture = chosen_mix,
      penalty = chosen,
      .metrics = list(outer_metrics),
      .coefs = list(tidy.coxnet_model(refit, penalty = chosen)),
      .inner = list(inner_metrics)
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

# The named arguments of `...` that cbe_loco_mp_coxnet() does not already take
# or set itself (B, mixture, penalty, eval_time, subject_id, ...), which it
# passes on to coxnet() and so to glmnet.
forwardable_glmnet_args <- function(args) {
  own <- names(formals(cbe_loco_mp_coxnet))
  nms <- names(args) %||% character(length(args))
  args[nzchar(nms) & !nms %in% own]
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
