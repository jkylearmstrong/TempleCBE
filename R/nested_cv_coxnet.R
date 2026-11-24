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
#' @section Nested CV vs Random-Alpha Design:
#' It is essential to select the appropriate validation and tuning workflow for your project goal:
#' \itemize{
#'   \item \strong{Nested Cross-Validation (\code{nested_cv_coxnet})}: Designed for \emph{unbiased generalization performance estimation}
#'     of the end-to-end modeling pipeline. Hyperparameters (\code{mixture} and \code{penalty}) are selected on inner resamples
#'     (e.g., using [rsample::group_vfold_cv()] or [rsample::group_bootstraps()]), and the refit model is scored on completely held-out
#'     outer assessment subjects. The chosen parameters may differ across outer splits, reflecting pipeline sensitivity and guarding
#'     against optimization bias.
#'   \item \strong{Random-Alpha Exploration (\code{tune_over_alpha} / \code{summarize_tune_results})}: Designed for
#'     \emph{exploratory hyperparameter tuning and model selection} across a continuous spectrum of \eqn{\alpha \in [0, 1]}. This workflow
#'     is ideal for comparing penalty properties (Ridge vs Lasso vs Elastic Net), profiling coefficient shrinkage paths across resamples,
#'     and choosing a single final model specification for reporting or deployment.
#' }
#'
#' @section Racing:
#' With `tune_method = "race_anova"`, how the inner resamples are raced depends
#' on the outcome:
#' \itemize{
#'   \item \strong{Start/stop data}, `Surv(start, stop, event)`, and gap mode:
#'     the race runs inside [cv_coxnet()] on glmnet directly (see "Racing" in
#'     [cv_coxnet()]). It uses finetune's elimination rule but scores at the
#'     subject level, like the grid search, so `metric`, `rule`, `penalty`,
#'     `covariates` and `recurrent` apply as they do for `"grid"`.
#'     [finetune::tune_race_anova()] cannot be used here: the yardstick
#'     survival metrics need a right-censored truth, and tune scores each
#'     start/stop row as if it were a subject.
#'   \item \strong{Right-censored data}, `Surv(time, status)`: the race is
#'     [finetune::tune_race_anova()] on a parsnip `proportional_hazards()`
#'     workflow, through [tune_race_survival()], selecting by `metric`
#'     (default `"brier_survival_integrated"`).
#' }
#' `"race_win_loss"` is available for right-censored data only.
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
#'   [loco_mp_coxnet()] is run on each outer analysis set at the `mixture`
#'   and penalty chosen there, with the same glmnet arguments (see `...`). It
#'   draws its minipatches by subject, so `group` does not apply to it, and a
#'   warning says so when `group` is set.
#' @param ... Further arguments passed to [glmnet::glmnet()], such as `cox.ties`,
#'   `standardize`, `penalty.factor`, or `nlambda`. With `importance = "loco_mp"`
#'   they are passed to [loco_mp_coxnet()] as well, so the minipatch models
#'   are fit like the tuned ones; arguments that [loco_mp_coxnet()] sets
#'   itself, such as `B`, are not forwarded.
#' @param loco_mp_B Number of minipatch ensembles drawn by [loco_mp_coxnet()]
#'   when `importance = "loco_mp"` (default `30`).
#' @param check_subject_overlap If `TRUE` (the default), stop when an outer
#'   split, or an inner resample, puts the same subject (or, when `group` is
#'   set, the same group) in both its analysis and its assessment set; see
#'   Details and [cv_coxnet()]. `FALSE` skips the check.
#' @param tune_method Method for tuning hyperparameters across the inner resamples:
#'   `"grid"` (default, standard [cv_coxnet()] grid search), `"race_anova"`
#'   (ANOVA racing), or `"race_win_loss"`
#'   ([finetune::tune_race_win_loss()] racing; needs BradleyTerry2). See
#'   "Racing" below.
#' @param control Racing settings for `tune_method = "race_anova"` or
#'   `"race_win_loss"`: a list or a [finetune::control_race()] object. See
#'   [cv_coxnet()] for the fields the start/stop race uses.
#' @param recurrent How recurrent (start/stop) data are scored, as in
#'   [cv_coxnet()]: `"error"` (default), `"first"`, or `"gap"` (gap-time
#'   concordance and gap-time Brier scores; needs `tune_method = "grid"` or
#'   `"race_anova"`).
#' @param checkpoint_dir Optional directory path where per-split checkpoints are stored.
#' @param shard Optional shard specification (e.g. `c(1, 2)`) for distributed computation.
#' @param checkpoint_mismatch Action on mismatch: `"recompute"`, `"overwrite"`, or `"error"`.
#' @inheritParams cv_coxnet
#' @return A tibble, of class `nested_cv_coxnet`, with one row per outer split:
#'   `id`, the chosen `mixture` and `penalty`, `.metrics` (outer assessment-set
#'   metrics), `.coefs` (coefficients of the refit), `.fits` (the refit itself,
#'   a [coxnet()] model at the split's chosen `mixture` and `penalty`), and
#'   `.inner` (the inner cross-validation's summarized metrics). If
#'   `importance = "loco_mp"`, includes `.importance` with tidy LOCO-MP
#'   statistical inference. When
#'   `tune_method` races, includes `.race`: a one-row tibble per outer split
#'   with `n_candidates`, `n_eliminated`, `n_survivors`, `n_resamples` (inner
#'   resamples) and `candidates` (one row per candidate with `mixture` and
#'   `penalty` — the finetune path lists only the tuned parameters —
#'   `n_resamples`, the inner resamples it was scored on, and `eliminated`). When a race produced no candidates, the row is a
#'   zero-count row (`n_candidates = 0` with an empty `candidates` table),
#'   never `NULL`, so `.race[[i]]` is always a one-row tibble. On
#'   counting-process data only `race_anova` is available (see "Racing");
#'   `race_win_loss` is for right-censored data only. The `.fits` column is what [predict_event_risk()]
#'   (and through it [calibration_from_model()] and
#'   [decision_curve_from_model()]) predicts from: a nested result has no
#'   single final model, so the risk at a horizon is one minus the mean of the
#'   outer splits' predicted survival at that horizon, each split at its own
#'   chosen `penalty`. `tune::collect_metrics()`
#'   averages `.metrics` over outer splits; [tidy_nested_cv_coxnet()] and
#'   [summarize_loco_mp()] unnest and summarize `.coefs` and `.importance`,
#'   and [tidy_outer_coefficients()], [summarize_outer_importance()] and
#'   [plot_outer_importance()] accept the result directly. [subject_nested_cv()]
#'   builds `object` with grouped folds stratified by subject event status.
#' @seealso [cv_coxnet()], [coxnet()], [loco_mp_coxnet()], [tune_race_survival()],
#'   [subject_nested_cv()]
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
                             trunc = 0.05, recurrent = c("error", "first", "gap"),
                             parallel = FALSE, importance = c("none", "loco_mp"),
                             loco_mp_B = 30L,
                             check_subject_overlap = TRUE,
                             tune_method = c("grid", "race_anova", "race_win_loss"),
                             control = NULL,
                             checkpoint_dir = NULL,
                             shard = NULL,
                             checkpoint_mismatch = c("recompute", "overwrite", "error"),
                             on_complete = getOption("templecbe.on_complete", NULL), ...) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  run_with_on_complete("nested_cv_coxnet", on_complete, meta = list(mixture = mixture), expr = {
  rlang::check_installed(
    c("glmnet", "survival", "rsample", "yardstick"),
    reason = "for nested cross-validation of `coxnet()` models."
  )
  if (!inherits(object, "nested_cv")) {
    stop("`object` must be an rsample::nested_cv() object.", call. = FALSE)
  }
  rule <- match.arg(rule)
  covariates <- match.arg(covariates)
  recurrent <- match.arg(recurrent)
  gap_mode <- identical(recurrent, "gap")
  importance <- match.arg(importance)
  tune_method <- match.arg(tune_method)
  if (gap_mode && is.null(metrics)) {
    metrics <- yardstick::metric_set(yardstick::concordance_survival)
  }
  metrics <- metrics %||% default_surv_metrics()
  if (gap_mode) {
    if (tune_method == "race_win_loss") {
      stop(
        "`recurrent = \"gap\"` needs `tune_method = \"grid\"` or `\"race_anova\"`: ",
        "win-loss racing runs on finetune, which cannot score gap times.",
        call. = FALSE
      )
    }
    gap_info <- surv_metric_info(metrics)
    if (!all(gap_info$metric %in% c("concordance_survival", "brier_survival_integrated", "brier_survival"))) {
      stop(
        "`recurrent = \"gap\"` scores gap-time concordance and gap-time Brier scores only; pass ",
        "`metrics` with `concordance_survival`, `brier_survival_integrated` or `brier_survival`. ",
        "`roc_auc_survival` is not available in gap mode.",
        call. = FALSE
      )
    }
  }
  info <- surv_metric_info(metrics)

  full_data <- object$splits[[1]]$data
  spec <- make_coxnet_spec(preprocessor, full_data, subject_id, group)
  # Start/stop outcomes (and gap mode) are raced inside cv_coxnet_impl() on
  # glmnet, with subject-level scoring; finetune races right-censored ones.
  outcome_type <- if (tune_method != "grid") surv_components(spec$fit_frame(full_data)$y)$type else NULL
  native_race <- tune_method == "race_anova" && (identical(outcome_type, "counting") || gap_mode)
  if (tune_method == "race_win_loss" && identical(outcome_type, "counting")) {
    stop(
      "`tune_method = \"race_win_loss\"` needs a right-censored outcome, Surv(time, status): it runs on ",
      "finetune, whose yardstick metrics cannot score start/stop data. Use `tune_method = \"race_anova\"`, ",
      "which races start/stop data on glmnet directly.",
      call. = FALSE
    )
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
    eval_time <- if (gap_mode) {
      default_gap_eval_time(
        surv_gap_truth(full$y, subject_keys(full$y, subject_values(full_data, spec)))
      )
    } else {
      default_eval_time(
        surv_subject_truth(full$y, subject_keys(full$y, subject_values(full_data, spec)), recurrent = recurrent)$.truth
      )
    }
  }

  run_outer <- function(i) with_quiet_cox_ties({
    outer <- object$splits[[i]]
    analysis <- rsample::analysis(outer)
    assessment <- rsample::assessment(outer)

    if (tune_method %in% c("race_anova", "race_win_loss") && !native_race) {
      rlang::check_installed(c("finetune", "parsnip", "workflows", "tune"), reason = "for racing in nested_cv_coxnet.")
      # `mixture` is tuned only when several values are supplied. The
      # `tune()` marker must appear inline in the spec: tune_args() only
      # detects `tune()` calls in the captured expression, so a variable
      # holding `tune()` left `mixture` unmarked and an explicit grid with
      # a `mixture` column was rejected (R7-04, mixture half). Each branch
      # builds its own spec so the fixed case stays untuned.
      spec_cox <- if (length(mixture) == 1) {
        parsnip::set_engine(
          parsnip::proportional_hazards(penalty = tune::tune(), mixture = mixture),
          "coxnet", ...
        ) |> parsnip::set_mode("censored regression")
      } else {
        parsnip::set_engine(
          parsnip::proportional_hazards(penalty = tune::tune(), mixture = tune::tune()),
          "coxnet", ...
        ) |> parsnip::set_mode("censored regression")
      }
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
        grid = race_explicit_grid(penalty, mixture),
        metrics = metrics,
        eval_time = eval_time,
        control = race_finetune_control(control)
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
      race_col <- race_summary_finetune(race_res, nrow(object$inner_resamples[[i]]))
    } else {
      inner <- cv_coxnet_impl(
        spec, data = analysis, mixture = mixture, penalty = penalty,
        resamples = object$inner_resamples[[i]], metrics = metrics, eval_time = eval_time,
        metric = metric, covariates = covariates, trunc = trunc, recurrent = recurrent, parallel = FALSE,
        check_subject_overlap = FALSE,
        tune_method = if (native_race) "race_anova" else "grid", control = control, ...
      )
      chosen_mix <- inner$mixture
      chosen <- if (rule == "min") inner$lambda_min else inner$lambda_1se
      refit <- inner$fit
      inner_metrics <- inner$metrics
      race_col <- if (native_race) race_summary(inner$race, nrow(object$inner_resamples[[i]])) else NULL
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
    if (gap_mode) {
      outer_parts <- list()
      if (any(info$metric == "concordance_survival")) {
        outer_parts <- c(outer_parts, list(score_gap_path(
          refit$fit, test$x, test$y, subject_test, penalty = chosen
        )))
      }
      brier_names <- intersect(info$metric, c("brier_survival_integrated", "brier_survival"))
      if (length(brier_names)) {
        brier_metrics <- yardstick_metric_set(brier_names)
        subject_train <- subject_keys(refit$y, subject_values(analysis, spec))
        outer_parts <- c(outer_parts, list(score_gap_brier_path(
          refit$fit, refit$x, refit$y, subject_train, test$x, test$y, subject_test,
          penalty = chosen, eval_time = eval_time, metrics = brier_metrics,
          info = surv_metric_info(brier_metrics), trunc = trunc
        )))
      }
      outer_metrics <- purrr::list_rbind(outer_parts)
      outer_metrics$penalty <- NULL
    } else {
      truth_train <- surv_subject_truth(refit$y, subject_keys(refit$y, subject_values(analysis, spec)), recurrent = recurrent)
      truth_test <- surv_subject_truth(test$y, subject_test, recurrent = recurrent)
      weights <- graf_weights(truth_test$.truth, eval_time, censoring_km(truth_train$.truth), trunc)

      outer_metrics <- score_coxnet_path(
        refit$fit, refit$x, refit$y, test$x, test$y, subject_test,
        penalty = chosen, eval_time = eval_time, truth = truth_test, weights = weights,
        metrics = metrics, info = info, covariates = covariates
      )
      outer_metrics$penalty <- NULL
    }

    loco_col <- if (importance == "loco_mp") {
      loco_fit <- tryCatch({
        do.call(loco_mp_coxnet, c(
          list(
            formula = preprocessor, data = analysis, subject_id = subject_id,
            mixture = chosen_mix, penalty = chosen, eval_time = eval_time,
            B = loco_mp_B, trunc = trunc, covariates = covariates, parallel = FALSE
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
      # The refit itself, at the split's chosen mixture and penalty: the
      # `.coefs` alone cannot predict (survival needs the fitted glmnet
      # object and the Breslow baseline from the split's training data),
      # so the refit is kept for predict_event_risk() and the validation
      # adapters. It holds the split's analysis-set design matrix and
      # outcome, so a nested result is larger than its metrics alone.
      .fits = list(refit),
      .inner = list(inner_metrics)
    )
    if (importance == "loco_mp") {
      res_row$.importance <- loco_col
    }
    if (tune_method != "grid") {
      res_row$.race <- list(race_col)
    }
    res_row
  })

  outer_ids <- seq_along(object$splits)
  n_outer <- length(outer_ids)

  if (!is.null(checkpoint_dir)) {
    ckpt_schema <- checkpoint_schema_version()
    data_fp <- paste0(as.character(openssl::sha256(serialize(full_data, NULL))), collapse = "")
    # The selection metric chooses the penalties, so the *effective*
    # metric belongs in the fingerprint (R7-01): `metric` as passed is
    # NULL for the default, and the default differs by tuning path — the
    # grid path selects by the first metric in `metrics`, the finetune
    # race by IBS.
    selection_metric <- if (tune_method %in% c("race_anova", "race_win_loss") && !native_race) {
      metric %||% "brier_survival_integrated"
    } else {
      metric %||% info$metric[1]
    }
    split_fps <- vapply(seq_len(n_outer), function(i) {
      payload <- list(
        schema = ckpt_schema,
        seed_scheme = current_seed_scheme(),
        outer_split = canonicalize_split_for_hash(object$splits[[i]]),
        inner_resamples = lapply(object$inner_resamples[[i]]$splits, canonicalize_split_for_hash),
        data = data_fp,
        rule = rule,
        mixture = mixture,
        penalty = penalty,
        tune_method = tune_method,
        control = if (tune_method == "grid") NULL else race_coxnet_control(control),
        # `race_coxnet_control()` drops `num_ties`, but the finetune
        # win-loss race uses it, so it is fingerprinted separately.
        control_num_ties = if (tune_method == "grid") NULL else race_control_num_ties(control),
        metric = selection_metric,
        # The predictor set is not in the data hash (the same data frame
        # feeds `~ x1 + x2` and `~ x1 + x3`), so the canonicalised
        # preprocessor belongs in the fingerprint (R7-02).
        preprocessor = canonicalize_preprocessor_for_hash(preprocessor),
        importance = importance,
        loco_mp_B = loco_mp_B,
        metrics = metric_set_names(metrics),
        eval_time = eval_time,
        covariates = covariates,
        recurrent = recurrent,
        trunc = trunc,
        glmnet_args = canonicalize_dots_for_hash(list(...)),
        # Rows now carry the outer refit in `.fits`. Checkpoints written
        # before that have no `.fits`, so they must not count as hits:
        # predict_event_risk() could not use them, and a re-run would keep
        # reusing them. This key makes them mismatch once and recompute.
        keeps_fits = TRUE
      )
      # The `templecbe.cox_ties` option is the fit-time default when no
      # explicit `cox.ties` is passed (see fit_glmnet_cox()), so flipping
      # it would otherwise reuse the old checkpoints (R6-02). The default
      # adds nothing here.
      if (!identical(get_templecbe_cox_ties(), "breslow")) payload$cox_ties <- get_templecbe_cox_ties()
      paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
    }, character(1))

    run_fp <- paste0(as.character(openssl::sha256(serialize(list(fps = split_fps, ids = object$id), NULL))), collapse = "")

    active_mask <- shard_active_mask(n_outer, validate_shard_spec(shard))
    ckpt_file <- function(i) {
      safe_id <- gsub("[^A-Za-z0-9._-]", "_", object$id[i])
      file.path(checkpoint_dir, sprintf("nested_coxnet_%s.rds", safe_id))
    }
    ckpt_meta <- function(i) {
      list(fn = "nested_cv_coxnet", fold_id = object$id[i], index = i)
    }
  } else {
    split_fps <- NULL
    run_fp <- NULL
    active_mask <- NULL
    ckpt_file <- NULL
    ckpt_meta <- NULL
  }

  rows <- run_checkpointed_sweep(
    compute = run_outer,
    n = n_outer,
    checkpoint_dir = checkpoint_dir,
    ckpt_file = ckpt_file,
    fingerprint = split_fps,
    run_fingerprint = run_fp,
    meta = ckpt_meta,
    active_mask = active_mask,
    parallel = isTRUE(parallel),
    furrr_seed = TRUE,
    skip_write_if_null = TRUE,
    parallel_reason = "to run outer splits in parallel."
  )

  out <- purrr::list_rbind(Filter(Negate(is.null), rows))
  if (!is.null(active_mask) && any(!active_mask) && sum(!vapply(rows, is.null, logical(1))) < n_outer) {
    warning(
      sprintf("nested_cv_coxnet(): shard %d/%d evaluated only %d of %d outer split(s); the result is partial. ",
              shard[1], shard[2], sum(!vapply(rows, is.null, logical(1))), n_outer),
      "Collect the other shards' checkpoints to complete it; see `attr(x, 'shard_partial')`.",
      call. = FALSE
    )
    attr(out, "shard_partial") <- list(
      shard = shard, n_outer = n_outer,
      n_evaluated = sum(!vapply(rows, is.null, logical(1))),
      evaluated_folds = object$id[!vapply(rows, is.null, logical(1))]
    )
  }
  class(out) <- c("nested_cv_coxnet", class(out))
  out
  })
}

# One-row summary of an inner race: how many candidates entered, how many
# were eliminated, and the per-candidate detail (`candidates`: mixture,
# penalty, n_resamples, eliminated). A race with no candidates still returns
# a one-row tibble — a zero-count row with an empty `candidates` table —
# never NULL, so `.race[[i]]` always has the documented shape.
race_summary <- function(candidates, n_resamples) {
  if (is.null(candidates) || !nrow(candidates)) {
    return(tibble::tibble(
      n_candidates = 0L,
      n_eliminated = 0L,
      n_survivors = 0L,
      n_resamples = as.integer(n_resamples),
      candidates = list(tibble::tibble(
        mixture = numeric(), penalty = numeric(),
        n_resamples = integer(), eliminated = logical()
      ))
    ))
  }
  tibble::tibble(
    n_candidates = nrow(candidates),
    n_eliminated = sum(candidates$eliminated),
    n_survivors = sum(!candidates$eliminated),
    n_resamples = as.integer(n_resamples),
    candidates = list(candidates)
  )
}

race_summary_finetune <- function(race_res, n_resamples) {
  m <- tryCatch(tune::collect_metrics(race_res), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(race_summary(NULL, n_resamples))
  params <- intersect(c("mixture", "penalty"), names(m))
  per <- dplyr::distinct(m[, c(params, ".config", "n"), drop = FALSE], .data$.config, .keep_all = TRUE)
  names(per)[names(per) == "n"] <- "n_resamples"
  per$n_resamples <- as.integer(per$n_resamples)
  per$eliminated <- per$n_resamples < max(per$n_resamples)
  # `.config` is only the join key for the per-candidate rows: the native
  # race path's candidates have no `.config`, so drop it to keep the two
  # `.race$candidates` schemas identical.
  per$.config <- NULL
  race_summary(tibble::as_tibble(per), n_resamples)
}

# The finetune grid, built from the user-supplied `penalty` (and
# `mixture`) values: only their length used to reach
# tune_race_survival(), so the race ran dials' own grid and the supplied
# values were silently discarded (R7-04). With no supplied penalty the
# dials grid size (20) is unchanged.
race_explicit_grid <- function(penalty, mixture) {
  if (!is.numeric(penalty)) {
    return(20)
  }
  pens <- sort(unique(penalty))
  if (is.numeric(mixture) && length(mixture) > 1) {
    expand.grid(penalty = pens, mixture = sort(unique(mixture)))
  } else {
    data.frame(penalty = pens)
  }
}

# A `control` for tune_race_survival(): NULL keeps its default; a plain list
# is turned into a finetune::control_race() object.
race_finetune_control <- function(control) {
  if (is.null(control) || inherits(control, "control_race")) {
    return(control)
  }
  if (!is.list(control)) {
    stop("`control` must be NULL, a list, or a finetune::control_race() object.", call. = FALSE)
  }
  do.call(control_race_survival, control)
}

# The named arguments of `...` that loco_mp_coxnet() does not already take
# or set itself (B, mixture, penalty, eval_time, subject_id, ...), which it
# passes on to coxnet() and so to glmnet.
forwardable_glmnet_args <- function(args) {
  own <- names(formals(loco_mp_coxnet))
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
