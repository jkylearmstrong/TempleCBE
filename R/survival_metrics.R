#' Integrated Brier Score of a Penalized Cox Model on Start/Stop Survival Data
#'
#' Tunes an elastic-net Cox model on the analysis set of one resampling split
#' and scores it on the assessment set by the integrated Brier score (IBS).
#' Lower is better. A convenience wrapper around [cv_coxnet()] for data in
#' counting-process (start/stop) layout, kept for existing analysis code; new
#' code can call [cv_coxnet()] and [nested_cv_coxnet()] directly.
#'
#' @details
#' 1. **Preprocessing.** `recipe` is prepped on the analysis set -- only each
#'    subject's first interval when `prep_on = "baseline"` -- and baked onto
#'    both sets. The recipe must keep `id_col`, `start_col`, `stop_col`, and
#'    `status_col` in its output (e.g. with an `"id variable"` role).
#' 2. **Tuning.** [cv_coxnet()] chooses the penalty on the baked analysis set
#'    with `internal_folds` folds grouped by subject, by `metric` (the IBS by
#'    default). Earlier versions used [glmnet::cv.glmnet()], which chose the
#'    penalty by concordance on folds of rows, splitting subjects' intervals
#'    between folds.
#' 3. **Scoring.** The model, refit on the whole analysis set, predicts each
#'    assessment-set subject's survival from the analysis-set Breslow baseline
#'    hazard along their covariate path (see [cv_coxnet()]), and is scored by
#'    [yardstick::brier_survival_integrated()] with inverse-probability-of-
#'    censoring (Graf) weights estimated on the analysis set.
#'
#' @param object An `rsplit` (e.g. one element of `rsample` resamples).
#' @param alpha Elastic net mixing parameter: 1 is lasso, 0 is ridge.
#' @param recipe An unprepped [recipes::recipe()].
#' @param feature_names Character vector of baked predictor column names, or a
#'   function that takes the baked analysis set and returns them -- for
#'   recipes whose output columns vary by fold, such as
#'   `step_pca(threshold = )`.
#' @param time_data Optional data frame with columns `start_col` and
#'   `stop_col`; its positive stop times are used as `eval_time` when that is
#'   `NULL`.
#' @param formula Optional character string of `+`-separated feature names
#'   (or a one-sided formula) to restrict the model to a subset of
#'   `feature_names`. Defaults to all of them.
#' @param internal_folds Number of subject-grouped folds used to choose the
#'   penalty.
#' @param id_col,start_col,stop_col,status_col Column names of the subject
#'   identifier, interval start, interval stop, and event indicator (1 = event).
#' @param censoring_weights Weighting scheme for Brier score calculation:
#'   `"ipcw"` (the default) uses inverse probability of censoring weighting via
#'   Graf weights (a proper scoring rule). `"none"` applies unweighted unit
#'   weights (1 for events and at-risk subjects, 0 for censored; legacy, emits
#'   a once-per-session warning; useful for historical reproducibility or
#'   sensitivity comparisons).
#' @param tune_engine Cross-validation tuning engine: `"yardstick"` (default, uses
#'   subject-grouped folds via [cv_coxnet()]) or `"cv.glmnet"` (legacy, splits
#'   intervals across folds without subject grouping using [glmnet::cv.glmnet()];
#'   emits a once-per-session warning, useful for historical reproducibility).
#' @param prep_on `"baseline"` (default) preps `recipe` on each subject's first
#'   interval in the analysis set; `"all"` preps on every analysis row.
#' @param eval_time Evaluation times for the IBS. Defaults to the stop times of
#'   `time_data`, or else deciles of the analysis-set event times.
#' @param metric Name of the yardstick survival metric that chooses the
#'   penalty, e.g. `"brier_survival_integrated"` or `"concordance_survival"`.
#' @param rule `"min"` or `"1se"`: report the penalty at `lambda.min` or
#'   `lambda.1se`.
#' @param covariates `"path"` or `"baseline"`; see [cv_coxnet()].
#' @param failure_ibs IBS reported when the model cannot be fit (default
#'   `NA`); a warning gives the reason.
#' @param parallel Logical or character. When \code{TRUE} (or \code{"resamples"} / \code{"everything"}),
#'   evaluates internal CV folds in parallel using [furrr::future_map2()] for \code{tune_engine = "yardstick"}
#'   or [glmnet::cv.glmnet()]'s parallel mode. When \code{FALSE} (default), evaluates sequentially.
#' @param parallel_over Tidymodels-compatible partition option: \code{"resamples"} (default) or \code{"everything"}.
#' @param ... Further arguments passed to [glmnet::glmnet()], such as
#'   `cox.ties` (`"breslow"` or `"efron"`). glmnet 5.0 defaults to Breslow and
#'   5.1 to Efron, so pass `cox.ties` to keep results stable across glmnet
#'   versions.
#' @return A tibble with columns `IBS`, `lambda`, `term`, `estimate`, and
#'   `alpha`: one row per feature (including those the penalty set to 0). When
#'   the fit fails, a single row with `IBS = failure_ibs`, `lambda = NA`, and
#'   `alpha`.
#' @references Graf E, Schmoor C, Sauerbrei W, Schumacher M (1999). Assessment
#'   and comparison of prognostic classification schemes for survival data.
#'   *Statistics in Medicine*, 18(17-18), 2529-2545.
#' @seealso [cv_coxnet()], [nested_cv_coxnet()], [tune_over_alpha()],
#'   [summarize_tune_results()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("yardstick", quietly = TRUE) &&
#'     requireNamespace("rsample", quietly = TRUE)) {
#'   set.seed(1)
#'   # Synthetic start/stop data: 60 subjects, up to 3 intervals each
#'   long <- do.call(rbind, lapply(1:60, function(i) {
#'     k <- sample(1:3, 1)
#'     stops <- c(5, 10, 20)[1:k]
#'     risk <- rnorm(1)
#'     data.frame(subject_id = i, tstart = c(0, head(stops, -1)), tstop = stops,
#'                status = c(rep(0, k - 1), rbinom(1, 1, plogis(risk))),
#'                x1 = risk + rnorm(k, sd = 0.2), x2 = rnorm(k))
#'   }))
#'   rec <- recipes::recipe(~ ., data = long) |>
#'     recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable") |>
#'     recipes::step_range(recipes::all_numeric_predictors())
#'   split <- rsample::group_initial_split(long, group = subject_id)
#'   glmnet_IBS(split, alpha = 0.5, recipe = rec, feature_names = c("x1", "x2"),
#'              time_data = unique(long[, c("tstart", "tstop")]), internal_folds = 3,
#'              id_col = "subject_id", cox.ties = "breslow")
#' }
#' }
glmnet_IBS <- function(object,
                       alpha = 1,
                       recipe,
                       feature_names,
                       time_data = NULL,
                       formula = NULL,
                       internal_folds = 5,
                       id_col = "id",
                       start_col = "tstart",
                       stop_col = "tstop",
                       status_col = "status",
                       censoring_weights = c("ipcw", "none"),
                       tune_engine = c("yardstick", "cv.glmnet"),
                       prep_on = c("baseline", "all"),
                       eval_time = NULL,
                       metric = "brier_survival_integrated",
                       rule = c("min", "1se"),
                       covariates = c("path", "baseline"),
                       failure_ibs = NA_real_,
                       parallel = FALSE,
                       parallel_over = c("resamples", "everything"),
                       ...) {
  censoring_weights <- match.arg(censoring_weights)
  if (censoring_weights == "none") {
    rlang::warn(
      "`censoring_weights = \"none\"` is legacy and does not adjust for censoring (not a proper Brier score). Use \"ipcw\" for proper scoring, or \"none\" only to reproduce historical runs or sensitivity comparisons.",
      .frequency = "once",
      .frequency_id = "TempleCBE_glmnet_IBS_censoring_weights_none"
    )
  }
  dots <- list(...)
  prepared_split <- dots$.prepared_split
  dots$.prepared_split <- NULL
  tune_engine <- match.arg(tune_engine)
  if (tune_engine == "cv.glmnet") {
    rlang::warn(
      "`tune_engine = \"cv.glmnet\"` is legacy and tunes without subject grouping. Use \"yardstick\" for grouped CV, or \"cv.glmnet\" only to reproduce historical runs.",
      .frequency = "once",
      .frequency_id = "TempleCBE_glmnet_IBS_tune_engine_cv_glmnet"
    )
  }
  prep_on <- match.arg(prep_on)
  rule <- match.arg(rule)
  covariates <- match.arg(covariates)
  is_parallel <- isTRUE(parallel) ||
    (is.character(parallel) && parallel %in% c("resamples", "everything", "true")) ||
    (is.list(parallel) && isTRUE(parallel$parallel_over %in% c("resamples", "everything"))) ||
    (!missing(parallel_over) && isTRUE(match.arg(parallel_over) %in% c("resamples", "everything")) && !identical(parallel, FALSE))
  if ("type.measure" %in% names(dots)) {
    type_measure <- dots$type.measure
    dots$type.measure <- NULL
    if (!identical(type_measure, "C")) {
      stop("`type.measure` is replaced by `metric`, the name of a yardstick survival metric.", call. = FALSE)
    }
    rlang::warn(
      "`type.measure = \"C\"` is deprecated; use `metric = \"concordance_survival\"`.",
      .frequency = "once", .frequency_id = "TempleCBE_glmnet_IBS_type_measure"
    )
    metric <- "concordance_survival"
  }

  require_packages(c("glmnet", "survival", "rsample", "yardstick", "recipes"), "glmnet_IBS")
  if (!inherits(object, "rsplit")) {
    stop("`object` must be an rsplit (e.g. one split of an rsample resample).", call. = FALSE)
  }
  if (!is.null(time_data)) {
    missing_time_cols <- setdiff(c(start_col, stop_col), names(time_data))
    if (length(missing_time_cols) > 0) {
      stop("`time_data` is missing column(s): ", paste(missing_time_cols, collapse = ", "), call. = FALSE)
    }
  }
  if (is.null(eval_time) && !is.null(time_data)) {
    eval_time <- sort(unique(time_data[[stop_col]]))
    eval_time <- eval_time[eval_time > 0]
  }
  if (!is.null(eval_time)) check_eval_time(eval_time)
  select_metrics <- yardstick_metric_set(metric)
  ibs_metrics <- yardstick_metric_set("brier_survival_integrated")

  if (!is.null(prepared_split) && isTRUE(prepared_split$rng_free)) {
    train <- prepared_split$train
    test <- prepared_split$test
  } else {
    train_raw <- rsample::analysis(object)
    test_raw <- rsample::assessment(object)
    missing_raw <- setdiff(c(id_col, start_col), names(train_raw))
    if (length(missing_raw) > 0) {
      stop("The data is missing column(s): ", paste(missing_raw, collapse = ", "), call. = FALSE)
    }
    prep_data <- if (prep_on == "baseline") {
      first_start <- stats::ave(train_raw[[start_col]], train_raw[[id_col]], FUN = min)
      train_raw[train_raw[[start_col]] == first_start, , drop = FALSE]
    } else {
      train_raw
    }
    recipes_was_attached <- "package:recipes" %in% search()
    on.exit({
      if (!recipes_was_attached && "package:recipes" %in% search()) {
        try(detach("package:recipes", unload = FALSE, character.only = TRUE), silent = TRUE)
      }
    }, add = TRUE)
    recipe <- bind_recipes_namespace(recipe)
    prepped <- recipes::prep(recipe, training = prep_data)
    train <- recipes::bake(prepped, new_data = train_raw)
    test <- recipes::bake(prepped, new_data = test_raw)
  }

  train_has_censoring <- if (all(c(id_col, status_col) %in% names(train))) {
    any(!tapply(train[[status_col]] != 0, train[[id_col]], any))
  } else if (status_col %in% names(train)) {
    any(train[[status_col]] == 0)
  } else {
    TRUE
  }
  if (!train_has_censoring) {
    freq <- if (isTRUE(getOption("templecbe_suppress_frequency_limit", FALSE))) "always" else "once"
    rlang::inform(
      "Training data contains no censored subjects; 'none' and 'ipcw' censoring weights produce identical scores.",
      .frequency = freq,
      .frequency_id = "TempleCBE_no_censored_subjects_notice"
    )
  }

  missing_keys <- setdiff(c(id_col, start_col, stop_col, status_col), names(train))
  if (length(missing_keys) > 0) {
    stop("The baked recipe output is missing column(s): ", paste(missing_keys, collapse = ", "),
         ". Keep them in the recipe with an \"id variable\" role.", call. = FALSE)
  }

  if (is.function(feature_names)) {
    feature_names <- feature_names(train)
  }
  if (is.null(formula)) {
    formula <- paste0(feature_names, collapse = " + ")
  }
  selected <- trimws(strsplit(formula_terms_string(formula), "\\+")[[1]])
  selected <- intersect(colnames(train), intersect(selected, feature_names))
  if (length(selected) == 0L) {
    stop("No requested `feature_names` were found in the baked training data.", call. = FALSE)
  }

  fit_and_score <- function() {
    if (!is.null(prepared_split) && isTRUE(prepared_split$rng_free)) {
      y_train <- prepared_split$y_train
      y_test <- prepared_split$y_test
      subject_train <- prepared_split$subject_train
      subject_test <- prepared_split$subject_test
      truth_train <- prepared_split$truth_train
      truth_test <- prepared_split$truth_test
      censor_km_train <- prepared_split$censor_km
    } else {
      y_train <- survival::Surv(train[[start_col]], train[[stop_col]], train[[status_col]])
      y_test <- survival::Surv(test[[start_col]], test[[stop_col]], test[[status_col]])
      # A bootstrap analysis set repeats subjects: score each copy as its own
      # subject, but keep a subject's copies in the same internal fold.
      subject_train <- subject_keys(y_train, train[[id_col]])
      subject_test <- subject_keys(y_test, test[[id_col]])
      truth_train <- surv_subject_truth(y_train, subject_train)
      truth_test <- surv_subject_truth(y_test, subject_test)
      censor_km_train <- NULL
    }

    if (tune_engine == "cv.glmnet") {
      x_mat <- as.matrix(train[selected])
      cv_args <- c(
        list(
          x = x_mat,
          y = y_train,
          family = "cox",
          alpha = alpha,
          nfolds = internal_folds,
          parallel = is_parallel
        ),
        dots
      )
      cv_res <- do.call(glmnet::cv.glmnet, cv_args)
      chosen <- if (rule == "min") cv_res$lambda.min else cv_res$lambda.1se
      fit_cox <- do.call(
        coxnet,
        c(
          list(
            x = as.data.frame(train[selected]),
            y = y_train,
            penalty = chosen,
            mixture = alpha
          ),
          dots
        )
      )
      effective_eval_time <- if (!is.null(eval_time)) eval_time else default_eval_time(truth_train$.truth)
      weights <- if (censoring_weights == "ipcw") {
        ckm <- censor_km_train %||% censoring_km(truth_train$.truth)
        graf_weights(truth_test$.truth, effective_eval_time, ckm)
      } else {
        w <- matrix(1, nrow = length(truth_test$.subject_id), ncol = length(effective_eval_time))
        dimnames(w) <- list(NULL, format(effective_eval_time))
        w
      }
      scored <- score_coxnet_path(
        fit_cox$fit, fit_cox$x, y_train, predictors_matrix(test[selected]), y_test, subject_test,
        penalty = chosen, eval_time = effective_eval_time, truth = truth_test, weights = weights,
        metrics = ibs_metrics, info = surv_metric_info(ibs_metrics), covariates = covariates
      )
      coefs <- tidy.coxnet_model(fit_cox, penalty = chosen)
      tibble::tibble(IBS = scored$.estimate[1], lambda = chosen, term = coefs$term, estimate = coefs$estimate, alpha = alpha)
    } else {
      cv <- do.call(cv_coxnet_impl, c(
        list(
          spec_xy(as.data.frame(train[selected]), y_train, subject_train, train[[id_col]]),
          mixture = alpha, v = internal_folds, metrics = select_metrics,
          eval_time = eval_time, metric = metric, covariates = covariates,
          parallel = is_parallel, parallel_over = parallel_over
        ),
        dots
      ))
      chosen <- if (rule == "min") cv$lambda_min else cv$lambda_1se

      weights <- if (censoring_weights == "ipcw") {
        ckm <- censor_km_train %||% censoring_km(truth_train$.truth)
        graf_weights(truth_test$.truth, cv$eval_time, ckm)
      } else {
        w <- matrix(1, nrow = length(truth_test$.subject_id), ncol = length(cv$eval_time))
        dimnames(w) <- list(NULL, format(cv$eval_time))
        w
      }
      scored <- score_coxnet_path(
        cv$fit$fit, cv$fit$x, y_train, predictors_matrix(test[selected]), y_test, subject_test,
        penalty = chosen, eval_time = cv$eval_time, truth = truth_test, weights = weights,
        metrics = ibs_metrics, info = surv_metric_info(ibs_metrics), covariates = covariates
      )
      coefs <- tidy.coxnet_model(cv$fit, penalty = chosen)
      tibble::tibble(IBS = scored$.estimate[1], lambda = chosen, term = coefs$term, estimate = coefs$estimate, alpha = alpha)
    }
  }

  tryCatch(fit_and_score(), error = function(e) {
    warning("glmnet_IBS() could not fit alpha = ", alpha, " and returned `failure_ibs`: ", conditionMessage(e), call. = FALSE)
    tibble::tibble(IBS = failure_ibs, lambda = NA_real_, alpha = alpha)
  })
}

# Process-local worker cache for shared data/recipes and pre-baked splits (D1)
.worker_cache <- new.env(parent = emptyenv())

#' Current Seed Scheme Identifier
#'
#' Returns the active task-seed generation scheme identifier (`"v0.5.6"`,
#' introduced when `summarize_tune_results()` switched to flat `split x alpha`
#' task seed pre-generation). Recorded in checkpoint fingerprints and run
#' manifests so checkpoints from earlier seed schemes are never mixed silently.
#'
#' @keywords internal
#' @noRd
current_seed_scheme <- function() {
  as.character(getOption("templecbe.seed_scheme", getOption("templecbe_seed_scheme", "v0.5.6")))
}

#' Resolve `checkpoint_mismatch` Option and Argument
#'
#' @keywords internal
#' @noRd
resolve_checkpoint_mismatch <- function(checkpoint_mismatch = NULL, missing_arg = FALSE) {
  valid_choices <- c("recompute", "overwrite", "error")
  if (isTRUE(missing_arg) || is.null(checkpoint_mismatch) || length(checkpoint_mismatch) > 1L) {
    opt <- getOption("templecbe.checkpoint_mismatch", getOption("templecbe_checkpoint_mismatch", NULL))
    if (!is.null(opt) && is.character(opt) && length(opt) == 1L && opt %in% valid_choices) {
      return(opt)
    }
    if (isTRUE(getOption("templecbe.checkpoint_overwrite", getOption("templecbe_checkpoint_overwrite", FALSE)))) {
      return("overwrite")
    }
    if (is.null(checkpoint_mismatch)) {
      return("recompute")
    }
    return(match.arg(checkpoint_mismatch, valid_choices))
  }
  match.arg(checkpoint_mismatch, valid_choices)
}

#' Execute a Long-Running Call With an Optional `on_complete` Hook (E6)
#'
#' @keywords internal
#' @noRd
run_with_on_complete <- function(fn_name = "TempleCBE",
                                 on_complete = getOption("templecbe.on_complete", getOption("templecbe_on_complete", NULL)),
                                 meta = list(),
                                 expr = NULL,
                                 fn = NULL) {
  fn_str <- as.character(fn %||% fn_name)[1]
  if (is.null(on_complete)) {
    on_complete <- getOption("templecbe.on_complete", getOption("templecbe_on_complete", NULL))
  }
  started_at <- Sys.time()
  t0 <- proc.time()[["elapsed"]]

  call_hook <- function(payload) {
    if (is.null(on_complete)) return(invisible(NULL))
    if (!is.function(on_complete)) {
      warning("`on_complete` must be a function (e.g. `function(result) ...`); ignoring.", call. = FALSE)
      return(invisible(NULL))
    }
    tryCatch({
      fmls <- names(formals(on_complete))
      if (length(fmls) >= 2L || "..." %in% fmls) {
        on_complete(payload, payload)
      } else {
        on_complete(payload)
      }
    }, error = function(he) {
      warning(sprintf("`on_complete` hook raised an error in %s(): %s", fn_str, conditionMessage(he)), call. = FALSE)
    })
  }

  res <- tryCatch(
    force(expr),
    error = function(e) {
      elapsed <- max(0, proc.time()[["elapsed"]] - t0)
      finished_at <- Sys.time()
      fail_obj <- structure(
        list(
          fn = fn_str,
          fn_name = fn_str,
          status = "failed",
          ok = FALSE,
          elapsed_sec = elapsed,
          started_at = started_at,
          finished_at = finished_at,
          timestamp = finished_at,
          result = NULL,
          error = e,
          error_message = conditionMessage(e),
          condition = e,
          meta = meta
        ),
        class = c("cbe_completion_event", "cbe_run_failure", "list"),
        status = "failed",
        fn = fn_str,
        elapsed_sec = elapsed
      )
      call_hook(fail_obj)
      stop(e)
    }
  )

  elapsed <- max(0, proc.time()[["elapsed"]] - t0)
  finished_at <- Sys.time()
  if (!is.null(res)) {
    tryCatch({
      attr(res, "run_status") <- "completed"
      attr(res, "elapsed_sec") <- elapsed
    }, error = function(e) NULL)
  }
  success_obj <- structure(
    list(
      fn = fn_str,
      fn_name = fn_str,
      status = "completed",
      ok = TRUE,
      elapsed_sec = elapsed,
      started_at = started_at,
      finished_at = finished_at,
      timestamp = finished_at,
      result = res,
      error = NULL,
      error_message = NULL,
      condition = NULL,
      meta = meta
    ),
    class = c("cbe_completion_event", "list"),
    status = "completed",
    fn = fn_str,
    elapsed_sec = elapsed
  )
  call_hook(success_obj)
  res
}

#' @export
`$.cbe_completion_event` <- function(x, name) {
  raw <- unclass(x)
  if (name %in% names(raw)) {
    return(raw[[name]])
  }
  res <- raw[["result"]]
  if (!is.null(res) && (is.list(res) || is.data.frame(res)) && name %in% names(res)) {
    return(res[[name]])
  }
  NULL
}

#' Pre-Prep and Bake a Split Once per Worker When RNG-Free (D1)
#'
#' @keywords internal
#' @noRd
prepare_split_for_ibs <- function(split, dots) {
  recipe <- dots$recipe
  if (!inherits(split, "rsplit") || !inherits(recipe, "recipe")) {
    return(NULL)
  }
  id_col <- dots$id_col %||% "id"
  start_col <- dots$start_col %||% "tstart"
  stop_col <- dots$stop_col %||% "tstop"
  status_col <- dots$status_col %||% "status"
  prep_on <- dots$prep_on %||% "baseline"

  train_raw <- rsample::analysis(split)
  test_raw <- rsample::assessment(split)
  if (!all(c(id_col, start_col) %in% names(train_raw))) {
    return(NULL)
  }

  prep_data <- if (prep_on == "baseline") {
    first_start <- stats::ave(train_raw[[start_col]], train_raw[[id_col]], FUN = min)
    train_raw[train_raw[[start_col]] == first_start, , drop = FALSE]
  } else {
    train_raw
  }

  has_seed_before <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  seed_before <- if (has_seed_before) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL

  recipes_was_attached <- "package:recipes" %in% search()
  on.exit({
    if (!recipes_was_attached && "package:recipes" %in% search()) {
      try(detach("package:recipes", unload = FALSE, character.only = TRUE), silent = TRUE)
    }
  }, add = TRUE)

  prepared <- tryCatch({
    rec_bound <- bind_recipes_namespace(recipe)
    prepped <- recipes::prep(rec_bound, training = prep_data)
    train <- recipes::bake(prepped, new_data = train_raw)
    test <- recipes::bake(prepped, new_data = test_raw)

    has_seed_after <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    seed_after <- if (has_seed_after) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
    rng_free <- identical(has_seed_before, has_seed_after) && identical(seed_before, seed_after)

    if (!all(c(id_col, start_col, stop_col, status_col) %in% names(train))) {
      return(NULL)
    }
    y_train <- survival::Surv(train[[start_col]], train[[stop_col]], train[[status_col]])
    y_test <- survival::Surv(test[[start_col]], test[[stop_col]], test[[status_col]])
    subject_train <- subject_keys(y_train, train[[id_col]])
    subject_test <- subject_keys(y_test, test[[id_col]])
    truth_train <- surv_subject_truth(y_train, subject_train)
    truth_test <- surv_subject_truth(y_test, subject_test)
    censor_km <- censoring_km(truth_train$.truth)

    list(
      train = train,
      test = test,
      y_train = y_train,
      y_test = y_test,
      subject_train = subject_train,
      subject_test = subject_test,
      truth_train = truth_train,
      truth_test = truth_test,
      censor_km = censor_km,
      rng_free = rng_free
    )
  }, error = function(e) {
    # Restore RNG state if prep failed midway
    if (has_seed_before) {
      assign(".Random.seed", seed_before, envir = globalenv())
    }
    NULL
  })

  prepared
}

#' Build a Compact Shared Bundle of Splits, Data, and Recipe for Worker Reuse (D1)
#'
#' Avoids duplicating `data` across every `rsplit` and `recipe$template`, and
#' writes the bundle once so each worker loads shared objects once per run.
#'
#' @keywords internal
#' @noRd
create_shared_worker_bundle <- function(splits, dots, run_fp) {
  shared_data <- if (length(splits) > 0L && inherits(splits[[1]], "rsplit") && is.data.frame(splits[[1]]$data)) {
    splits[[1]]$data
  } else {
    NULL
  }

  compact_splits <- lapply(splits, function(sp) {
    if (!is.null(shared_data) && inherits(sp, "rsplit") && identical(sp$data, shared_data)) {
      sp_copy <- sp
      sp_copy$data <- NULL
      list(is_compact = TRUE, split = sp_copy)
    } else {
      list(is_compact = FALSE, split = sp)
    }
  })

  dots_compact <- dots
  recipe_compact <- NULL
  recipe_uses_shared_data <- FALSE
  if (!is.null(dots_compact$recipe) && inherits(dots_compact$recipe, "recipe")) {
    rec <- bind_recipes_namespace(dots_compact$recipe)
    if (!is.null(shared_data) && identical(rec$template, shared_data)) {
      rec$template <- shared_data[0, , drop = FALSE]
      recipe_uses_shared_data <- TRUE
    }
    recipe_compact <- rec
    dots_compact$recipe <- NULL
  }

  payload <- list(
    run_fp = run_fp,
    shared_data = shared_data,
    compact_splits = compact_splits,
    recipe_compact = recipe_compact,
    recipe_uses_shared_data = recipe_uses_shared_data,
    dots_compact = dots_compact
  )

  bundle_file <- tempfile(pattern = "tcbe_shared_bundle_", fileext = ".rds")
  tryCatch(saveRDS(payload, bundle_file, compress = FALSE), error = function(e) NULL)

  # Seed the current process's `.worker_cache` as well so sequential execution has zero file I/O
  hydrated <- hydrate_worker_bundle(payload)
  assign(run_fp, hydrated, envir = .worker_cache)

  list(
    bundle_file = bundle_file,
    payload = payload
  )
}

#' @noRd
hydrate_worker_bundle <- function(payload) {
  shared_data <- payload$shared_data
  splits <- lapply(payload$compact_splits, function(cs) {
    sp <- cs$split
    if (isTRUE(cs$is_compact)) {
      sp$data <- shared_data
    }
    sp
  })
  dots <- payload$dots_compact
  if (!is.null(payload$recipe_compact)) {
    rec <- payload$recipe_compact
    if (isTRUE(payload$recipe_uses_shared_data) && !is.null(shared_data)) {
      rec$template <- shared_data
    }
    dots$recipe <- rec
  }
  list(
    splits = splits,
    dots = dots,
    prepared_splits = new.env(parent = emptyenv())
  )
}

#' Retrieve (and Cache Once per Worker) Hydrated Shared Objects and Split Prep (D1)
#'
#' @keywords internal
#' @noRd
get_worker_bundle <- function(run_fp, bundle_file = NULL, fallback_payload = NULL) {
  if (exists(run_fp, envir = .worker_cache, inherits = FALSE)) {
    return(get(run_fp, envir = .worker_cache, inherits = FALSE))
  }
  payload <- NULL
  if (!is.null(bundle_file) && file.exists(bundle_file)) {
    payload <- tryCatch(readRDS(bundle_file), error = function(e) NULL)
  }
  if (is.null(payload)) {
    payload <- fallback_payload
  }
  if (is.null(payload)) {
    stop("Worker could not load shared data bundle for run ", run_fp, ".", call. = FALSE)
  }
  # Evict old runs in worker cache to bound worker memory
  existing_keys <- ls(envir = .worker_cache, all.names = TRUE)
  if (length(existing_keys) >= 4L) {
    rm(list = existing_keys, envir = .worker_cache)
  }
  hydrated <- hydrate_worker_bundle(payload)
  assign(run_fp, hydrated, envir = .worker_cache)
  hydrated
}

#' @noRd
get_worker_prepared_split <- function(bundle, sp_idx) {
  key <- as.character(sp_idx)
  if (exists(key, envir = bundle$prepared_splits, inherits = FALSE)) {
    return(get(key, envir = bundle$prepared_splits, inherits = FALSE))
  }
  prep_obj <- prepare_split_for_ibs(bundle$splits[[sp_idx]], bundle$dots)
  if (!is.null(prep_obj) && isTRUE(prep_obj$rng_free)) {
    assign(key, prep_obj, envir = bundle$prepared_splits)
  }
  prep_obj
}

#' Bind `recipes` Namespace Exports Into Step Quosure Environments
#'
#' Ensures unqualified selector calls such as `all_numeric_predictors()` inside
#' recipe steps resolve against the `recipes` namespace without attaching
#' `package:recipes` to the user's `search()` path.
#'
#' @param recipe A `recipe` object.
#' @return The `recipe` object with shimmed step quosure environments.
#' @keywords internal
#' @noRd
bind_recipes_namespace <- function(recipe) {
  if (!inherits(recipe, "recipe") || is.null(recipe$steps) || !requireNamespace("recipes", quietly = TRUE)) {
    return(recipe)
  }
  rec_ns <- asNamespace("recipes")
  rec_exports <- getNamespaceExports("recipes")
  rec_list <- mget(rec_exports, envir = rec_ns, inherits = TRUE)
  shim_quosure <- function(q) {
    if (rlang::is_quosure(q)) {
      orig_env <- rlang::quo_get_env(q)
      if (!identical(orig_env, emptyenv())) {
        if (!isTRUE(attr(orig_env, "tcbe_recipes_shimmed", exact = TRUE))) {
          shim_env <- list2env(rec_list, parent = orig_env)
          attr(shim_env, "tcbe_recipes_shimmed") <- TRUE
          q <- rlang::quo_set_env(q, shim_env)
        }
      }
    }
    q
  }
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    if (!is.null(step$terms) && is.list(step$terms)) {
      recipe$steps[[i]]$terms <- lapply(step$terms, shim_quosure)
    }
  }
  recipe
}

#' Canonicalize a Split for Checkpoint Fingerprinting
#'
#' @keywords internal
#' @noRd
canonicalize_split_for_hash <- function(split) {
  if (!inherits(split, "rsplit")) {
    return(list(class = class(split)))
  }
  in_id <- split$in_id
  out_id <- tryCatch(rsample::complement(split), error = function(e) split$out_id)
  data_hash <- if (is.data.frame(split$data)) {
    openssl::sha256(serialize(list(
      dim = dim(split$data),
      names = names(split$data),
      in_rows = split$data[in_id, , drop = FALSE],
      out_rows = if (!is.null(out_id) && all(!is.na(out_id))) split$data[out_id, , drop = FALSE] else NULL
    ), NULL))
  } else {
    NA_character_
  }
  list(
    in_id = as.integer(in_id),
    out_id = if (!is.null(out_id)) as.integer(out_id) else NULL,
    id = split$id,
    data_hash = as.character(data_hash)
  )
}

#' Canonicalize a Recipe for Checkpoint Fingerprinting
#'
#' @keywords internal
#' @noRd
canonicalize_recipe_for_hash <- function(recipe) {
  if (!inherits(recipe, "recipe")) {
    return(recipe)
  }
  steps_canon <- lapply(recipe$steps, function(st) {
    st_clean <- lapply(names(st), function(nm) {
      val <- st[[nm]]
      if (nm == "terms" && is.list(val)) {
        vapply(val, rlang::quo_text, character(1))
      } else if (rlang::is_quosure(val)) {
        rlang::quo_text(val)
      } else if (is.function(val)) {
        paste(deparse(val), collapse = "\n")
      } else if (is.environment(val)) {
        "<env>"
      } else {
        val
      }
    })
    names(st_clean) <- names(st)
    list(class = class(st), fields = st_clean)
  })
  list(
    var_info = if (!is.null(recipe$var_info)) as.data.frame(recipe$var_info) else NULL,
    term_info = if (!is.null(recipe$term_info)) as.data.frame(recipe$term_info) else NULL,
    steps = steps_canon
  )
}

#' Canonicalize Dots / Settings for Checkpoint Fingerprinting
#'
#' @keywords internal
#' @noRd
canonicalize_dots_for_hash <- function(dots) {
  defaults <- list(
    censoring_weights = "ipcw",
    internal_folds = 5,
    id_col = "id",
    start_col = "tstart",
    stop_col = "tstop",
    status_col = "status",
    tune_engine = "yardstick",
    prep_on = "baseline",
    eval_time = NULL,
    metric = "brier_survival_integrated",
    rule = "min",
    covariates = "path"
  )
  merged <- utils::modifyList(defaults, dots, keep.null = TRUE)
  # Remove execution-only flags that do not change model outputs
  merged$progress <- NULL
  merged$parallel <- NULL
  merged$parallel_over <- NULL
  merged$checkpoint_dir <- NULL
  merged$checkpoint_mismatch <- NULL
  merged$shard <- NULL
  merged$chunk_size <- NULL
  merged$on_complete <- NULL
  merged$.prepared_split <- NULL

  if (!is.null(merged$recipe)) {
    merged$recipe <- canonicalize_recipe_for_hash(merged$recipe)
  }
  if (is.function(merged$feature_names)) {
    merged$feature_names <- paste(deparse(merged$feature_names), collapse = "\n")
  } else if (!is.null(merged$feature_names)) {
    merged$feature_names <- as.character(merged$feature_names)
  }
  if (!is.null(merged$formula)) {
    merged$formula <- formula_terms_string(merged$formula)
  }
  nms <- sort(names(merged))
  merged[nms]
}

#' Compute SHA-256 Checkpoint Fingerprint
#'
#' @keywords internal
#' @noRd
compute_checkpoint_fingerprint <- function(split = NULL,
                                           dots = list(),
                                           alpha = NULL,
                                           formula = NULL,
                                           seed = NULL,
                                           extra = list(),
                                           seed_scheme = current_seed_scheme()) {
  pkg_ver <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.12")
  payload <- list(
    version = pkg_ver,
    seed_scheme = as.character(seed_scheme),
    split = if (!is.null(split)) canonicalize_split_for_hash(split) else NULL,
    settings = canonicalize_dots_for_hash(dots),
    alpha = if (!is.null(alpha)) round(as.numeric(alpha), 12) else NULL,
    formula = if (!is.null(formula) && !all(is.na(formula))) formula_terms_string(formula) else NULL,
    seed = seed,
    extra = extra
  )
  paste0(as.character(openssl::sha256(serialize(payload, NULL))), collapse = "")
}

#' Read and Verify a Checkpoint RDS File
#'
#' @keywords internal
#' @noRd
read_verified_checkpoint <- function(ckpt_file,
                                     expected_fp,
                                     checkpoint_mismatch = c("recompute", "overwrite", "error")) {
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
  if (is.null(ckpt_file) || !file.exists(ckpt_file)) {
    return(list(status = "missing", value = NULL))
  }
  cached <- tryCatch(readRDS(ckpt_file), error = function(e) NULL)
  if (is.null(cached)) {
    return(list(status = "invalid", value = NULL))
  }

  is_env <- inherits(cached, "templecbe_checkpoint_envelope") ||
    (is.list(cached) && !is.data.frame(cached) && all(c("value", "checkpoint_fingerprint") %in% names(cached)))

  if (is_env) {
    found_fp <- cached$checkpoint_fingerprint
    found_scheme <- cached$seed_scheme %||% cached$checkpoint_meta$seed_scheme
    val <- cached$value
  } else {
    found_fp <- attr(cached, "checkpoint_fingerprint", exact = TRUE)
    found_scheme <- attr(cached, "seed_scheme", exact = TRUE) %||%
      attr(cached, "checkpoint_meta", exact = TRUE)$seed_scheme
    val <- cached
    attr(val, "checkpoint_fingerprint") <- NULL
    attr(val, "checkpoint_run_fingerprint") <- NULL
    attr(val, "seed_scheme") <- NULL
    attr(val, "checkpoint_meta") <- NULL
  }

  if (is.data.frame(val) && nrow(val) == 0L) {
    return(list(status = "invalid", value = NULL))
  }

  expected_scheme <- current_seed_scheme()
  scheme_ok <- is.null(found_scheme) || identical(as.character(found_scheme), expected_scheme)

  if (identical(as.character(found_fp), as.character(expected_fp)) && scheme_ok) {
    return(list(status = "hit", value = val))
  }
  msg <- sprintf(
    "Checkpoint '%s' has a mismatched fingerprint (inputs, recipe, features, censoring_weights, seed_scheme, or settings changed).",
    basename(ckpt_file)
  )
  if (checkpoint_mismatch == "error") {
    stop(msg, call. = FALSE)
  }
  list(status = "mismatch", value = NULL, message = msg)
}

#' Save a Checkpoint RDS File With Fingerprint Metadata
#'
#' @keywords internal
#' @noRd
strip_checkpoint_attrs <- function(x) {
  if (is.null(x)) return(NULL)
  attr(x, "checkpoint_fingerprint") <- NULL
  attr(x, "checkpoint_run_fingerprint") <- NULL
  attr(x, "seed_scheme") <- NULL
  attr(x, "checkpoint_meta") <- NULL
  x
}

write_verified_checkpoint <- function(obj,
                                      ckpt_file,
                                      fingerprint,
                                      run_fingerprint = fingerprint,
                                      meta = list()) {
  seed_scheme <- meta$seed_scheme %||% current_seed_scheme()
  attr(obj, "checkpoint_fingerprint") <- as.character(fingerprint)
  attr(obj, "checkpoint_run_fingerprint") <- as.character(run_fingerprint)
  attr(obj, "seed_scheme") <- as.character(seed_scheme)
  meta$task_fingerprint <- as.character(fingerprint)
  meta$run_fingerprint <- as.character(run_fingerprint)
  meta$seed_scheme <- as.character(seed_scheme)
  meta$version <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.12")
  attr(obj, "checkpoint_meta") <- meta
  if (!is.null(ckpt_file)) {
    tryCatch(saveRDS(obj, ckpt_file), error = function(e) NULL)
  }
  obj
}

#' Validate a `shard = c(i, n)` Specification
#'
#' @keywords internal
#' @noRd
validate_shard_spec <- function(shard) {
  if (is.null(shard)) {
    return(NULL)
  }
  if (is.character(shard) && length(shard) == 1L && grepl("^\\s*\\d+\\s*/\\s*\\d+\\s*$", shard)) {
    shard <- as.integer(trimws(strsplit(shard, "/")[[1]]))
  }
  if (!is.numeric(shard) || length(shard) != 2L || any(is.na(shard)) ||
      any(shard != round(shard)) || shard[2] < 1 || shard[1] < 1 || shard[1] > shard[2]) {
    stop("`shard` must be an integer vector `c(i, n)` or string 'i/n' with `1 <= i <= n`.", call. = FALSE)
  }
  as.integer(shard)
}

#' @noRd
furrr_worker_packages <- function(pkgs = c("rsample", "yardstick", "survival", "glmnet", "dplyr", "TempleCBE")) {
  if (!requireNamespace("future", quietly = TRUE) || inherits(future::plan(), "sequential")) {
    return(character(0))
  }
  Filter(function(p) nzchar(system.file(package = p)), pkgs)
}

#' Tune a Penalized Cox Model Over a Grid of `alpha` Values
#'
#' Runs [glmnet_IBS()] on one resampling split for each value in an `alpha`
#' grid, or for each of a set of candidate formulas, in parallel via
#' [furrr::future_map()]. Set a [future::plan()] first to run in parallel;
#' this function never sets one.
#'
#' @details
#' **Grid mode** (the default). The grid is `num_fixed` evenly spaced values
#' from 0 to 1, plus `num_alpha_values - num_fixed` values drawn uniformly at
#' random, one per gap between consecutive fixed values (cycling through the
#' gaps if there are more random draws than gaps).
#'
#' **Formula mode** (`formulas` given). One fit per formula, each with its own
#' `alpha`: the matching element of `alphas`, or a value drawn uniformly from
#' 0 to 1 when `alphas` is `NULL`. Each result gains a `formula` column.
#'
#' Random draws use the R session's random number stream, so call
#' [set.seed()] first for reproducible values. Model fits run with
#' `furrr_options(seed = TRUE)`, so they are reproducible too.
#'
#' To tune `alpha` and the penalty together in one call, see [cv_coxnet()].
#'
#' @param object An `rsplit`.
#' @param ... Arguments passed to [glmnet_IBS()] (`recipe`, `feature_names`,
#'   `time_data`, `id_col`, `cox.ties`, ...).
#' @param num_alpha_values Total number of `alpha` values in the grid.
#' @param num_fixed Number of evenly spaced values from 0 to 1 in the grid.
#' @param alphas Optional numeric vector of `alpha` values to use instead of
#'   the generated grid; in formula mode, one per formula.
#' @param formulas Optional character vector of `+`-separated feature sets, or
#'   a list of one-sided formulas, each fit as a separate model (see
#'   [glmnet_IBS()]'s `formula`).
#' @param progress Show a progress bar.
#' @param seed Optional integer seed for reproducible random draws of `alpha`
#'   and formulas.
#' @param checkpoint_dir Optional directory path to save per-alpha checkpoint files.
#'   Finished fits are loaded from disk on rerun, skipping recomputation when
#'   their checkpoint fingerprint matches.
#' @param shard Optional integer vector `c(i, n)` or string `'i/n'` specifying
#'   that this execution evaluates shard `i` of `n` items.
#' @param checkpoint_mismatch Action when a checkpoint file exists in
#'   `checkpoint_dir` but its stored fingerprint does not match the current
#'   inputs/settings: `"recompute"` (default; warns and recomputes the task),
#'   `"overwrite"` (overwrites mismatched files in place and cleans up stale
#'   checkpoint files from previous runs), or `"error"` (stops immediately).
#' @param on_complete Optional callback `function(result)` (or option
#'   `templecbe.on_complete`) invoked when the run finishes or errors.
#' @return A list named by `alpha`, one element per fit, each the output of
#'   [purrr::safely()]: a list with `result` (the [glmnet_IBS()] tibble, or
#'   `NULL`) and `error` (`NULL`, or the condition).
#' @seealso [glmnet_IBS()], [summarize_tune_results()], [clean_checkpoints()], [cv_coxnet()]
#' @export
tune_over_alpha <- function(object,
                            ...,
                            num_alpha_values = 10,
                            num_fixed = 6,
                            alphas = NULL,
                            formulas = NULL,
                            progress = FALSE,
                            seed = NULL,
                            checkpoint_dir = NULL,
                            checkpoint_mismatch = c("recompute", "overwrite", "error"),
                            shard = NULL,
                            on_complete = getOption("templecbe.on_complete", NULL)) {
  run_with_on_complete(on_complete = on_complete, fn = "tune_over_alpha", expr = {
    require_packages("furrr", "tune_over_alpha")
    checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
    dots <- list(...)
    if (!is.null(seed)) {
      local_seed(seed)
    }

    if (!is.null(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    }

    furrr_pkgs <- furrr_worker_packages()

    if (!is.null(formulas)) {
      if ("formula" %in% names(dots)) {
        stop("Pass either `formulas` or `formula`, not both.", call. = FALSE)
      }
      if (inherits(formulas, "formula")) {
        formulas <- list(formulas)
      }
      formulas <- vapply(formulas, formula_terms_string, character(1), USE.NAMES = FALSE)
      if (is.null(alphas)) {
        alphas <- stats::runif(length(formulas))
      }
      if (length(alphas) != length(formulas)) {
        stop("`alphas` must have one value per element of `formulas`.", call. = FALSE)
      }
      run_fp <- compute_checkpoint_fingerprint(
        split = object, dots = dots, seed = seed,
        extra = list(fn = "tune_over_alpha", alphas = round(alphas, 12), formulas = formulas)
      )
      task_fps <- Map(function(a, f) {
        compute_checkpoint_fingerprint(split = object, dots = dots, alpha = a, formula = f, seed = seed)
      }, alphas, formulas)
      ckpt_files <- Map(function(a, f) {
        if (is.null(checkpoint_dir)) return(NULL)
        safe_a <- gsub("[^A-Za-z0-9._-]", "_", as.character(round(a, 6)))
        safe_f <- gsub("[^A-Za-z0-9._-]", "_", as.character(f))
        file.path(checkpoint_dir, sprintf("tune_alpha_%s_%s.rds", safe_a, safe_f))
      }, alphas, formulas)

      if (!is.null(checkpoint_dir)) {
        checks <- Map(read_verified_checkpoint, ckpt_files, task_fps,
                      MoreArgs = list(checkpoint_mismatch = checkpoint_mismatch))
        mismatches <- vapply(checks, function(ch) identical(ch$status, "mismatch"), logical(1))
        if (any(mismatches)) {
          for (mf in unlist(ckpt_files[mismatches])) {
            if (file.exists(mf)) unlink(mf)
          }
          if (checkpoint_mismatch != "overwrite") {
            warning(sprintf(
              "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed.",
              sum(mismatches), checkpoint_dir
            ), call. = FALSE)
          }
        }
      }

      shard_parsed <- validate_shard_spec(shard)
      active_mask <- if (!is.null(shard_parsed)) {
        ((seq_along(alphas) - 1L) %% shard_parsed[2]) + 1L == shard_parsed[1]
      } else {
        rep(TRUE, length(alphas))
      }

      runs <- furrr::future_pmap(
        list(a = alphas, f = formulas, ckpt_file = ckpt_files, task_fp = task_fps, item_idx = seq_along(alphas), is_active = active_mask),
        purrr::safely(function(a, f, ckpt_file, task_fp, item_idx, is_active) {
          if (!is.null(ckpt_file) && file.exists(ckpt_file)) {
            chk <- read_verified_checkpoint(ckpt_file, task_fp, checkpoint_mismatch = "recompute")
            if (identical(chk$status, "hit")) {
              return(chk$value)
            }
          }
          if (!is_active) {
            return(NULL)
          }
          res <- do.call(glmnet_IBS, c(list(object = object, alpha = a, formula = f), dots))
          res$formula <- f
          res <- write_verified_checkpoint(
            res, ckpt_file, fingerprint = task_fp, run_fingerprint = run_fp,
            meta = list(fn = "tune_over_alpha", task_idx = item_idx, n_tasks = length(alphas), alpha = a, formula = f)
          )
          res
        }),
        .options = furrr::furrr_options(seed = TRUE, packages = furrr_pkgs),
        .progress = progress
      )
      if (!is.null(checkpoint_dir) && checkpoint_mismatch == "overwrite") {
        clean_checkpoints(checkpoint_dir, keep = "current", run_fingerprint = run_fp)
      }
      names(runs) <- alphas
      return(runs)
    }

    if (is.null(alphas)) {
      alphas <- alpha_grid(num_alpha_values, num_fixed)
    }
    alphas <- sort(alphas)
    names(alphas) <- alphas

    run_fp <- compute_checkpoint_fingerprint(
      split = object, dots = dots, seed = seed,
      extra = list(fn = "tune_over_alpha", alphas = round(as.numeric(alphas), 12))
    )
    task_fps <- lapply(alphas, function(a) {
      compute_checkpoint_fingerprint(split = object, dots = dots, alpha = a, seed = seed)
    })
    ckpt_files <- lapply(alphas, function(a) {
      if (is.null(checkpoint_dir)) return(NULL)
      safe_a <- gsub("[^A-Za-z0-9._-]", "_", as.character(round(a, 6)))
      file.path(checkpoint_dir, sprintf("tune_alpha_%s.rds", safe_a))
    })

    if (!is.null(checkpoint_dir)) {
      checks <- Map(read_verified_checkpoint, ckpt_files, task_fps,
                    MoreArgs = list(checkpoint_mismatch = checkpoint_mismatch))
      mismatches <- vapply(checks, function(ch) identical(ch$status, "mismatch"), logical(1))
      if (any(mismatches)) {
        for (mf in unlist(ckpt_files[mismatches])) {
          if (file.exists(mf)) unlink(mf)
        }
        if (checkpoint_mismatch != "overwrite") {
          warning(sprintf(
            "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed.",
            sum(mismatches), checkpoint_dir
          ), call. = FALSE)
        }
      }
    }

    shard_parsed <- validate_shard_spec(shard)
    active_mask <- if (!is.null(shard_parsed)) {
      ((seq_along(alphas) - 1L) %% shard_parsed[2]) + 1L == shard_parsed[1]
    } else {
      rep(TRUE, length(alphas))
    }

    runs <- furrr::future_pmap(
      list(a = as.numeric(alphas), ckpt_file = ckpt_files, task_fp = task_fps, item_idx = seq_along(alphas), is_active = active_mask),
      purrr::safely(function(a, ckpt_file, task_fp, item_idx, is_active) {
        if (!is.null(ckpt_file) && file.exists(ckpt_file)) {
          chk <- read_verified_checkpoint(ckpt_file, task_fp, checkpoint_mismatch = "recompute")
          if (identical(chk$status, "hit")) {
            return(chk$value)
          }
        }
        if (!is_active) {
          return(NULL)
        }
        res <- do.call(glmnet_IBS, c(list(object = object, alpha = a), dots))
        res <- write_verified_checkpoint(
          res, ckpt_file, fingerprint = task_fp, run_fingerprint = run_fp,
          meta = list(fn = "tune_over_alpha", task_idx = item_idx, n_tasks = length(alphas), alpha = a)
        )
        res
      }),
      .options = furrr::furrr_options(seed = TRUE, packages = furrr_pkgs),
      .progress = progress
    )
    if (!is.null(checkpoint_dir) && checkpoint_mismatch == "overwrite") {
      clean_checkpoints(checkpoint_dir, keep = "current", run_fingerprint = run_fp)
    }
    names(runs) <- names(alphas)
    runs
  })
}

#' Tune Over `alpha` for Every Split of a Resample
#'
#' Evaluates candidate `(split, alpha)` (or `(split, formula)`) combinations
#' as a single flat task grid across all available [future::plan()] workers and
#' row-binds the successful fits.
#'
#' @section Nested Parallelism, Benchmarking & Physical Cores:
#' When tuning penalized survival models across nested resampling structures, computation
#' scales multiplicatively: \eqn{\text{outer folds} \times \text{inner splits} \times \text{alphas} \times \text{internal CV folds}}.
#' `tune_alpha_by_split()` flattens `inner splits x alphas` into a single task
#' pool (e.g. 7 splits x 10 alphas = 70 tasks), sends the shared dataset, recipe,
#' and split definitions once per worker, and pre-preps/bakes each split once per
#' worker rather than repeating recipe preprocessing for every `alpha`.
#'
#' **Physical Cores vs. Hyper-Threading:** When configuring a parallel backend
#' (e.g. `future::plan(future::multisession, workers = ...)`), it is strongly
#' recommended to set worker count to the number of *physical* CPU cores
#' ([parallel::detectCores(logical = FALSE)]), leaving 1--2 cores for the coordinator
#' process and OS I/O, rather than using all hyper-threaded logical cores.
#' Because CPU-intensive coordinate descent and Breslow hazard calculations saturate
#' CPU execution units and L1/L2 caches, two worker processes executing on siblings of
#' the same physical core contend for the same ALUs, FPUs, and memory bandwidth, which
#' frequently doubles per-task compute latency inside concurrent workers (e.g., tasks
#' taking ~2.2 s serially taking ~5.0 s inside 14 concurrent workers on 8 physical cores).
#' Sizing workers to physical cores yields optimal wall-clock speedup and reduces IPC
#' contention. Set \code{future::plan(multisession, workers = 7)} before calling while
#' keeping inner model evaluations sequential (\code{parallel = FALSE} in \code{glmnet_IBS()}).
#' Use `chunk_size` to control how many tasks are batched per worker message.
#'
#' @param object A resampling object with a `splits` column (e.g. one element
#'   of the `inner_resamples` column of [rsample::nested_cv()]).
#' @inheritParams tune_over_alpha
#' @param seed Optional integer seed for reproducible random draws across splits.
#' @param calibrate Logical (default TRUE); whether to benchmark baseline serial execution.
#'   Set to FALSE to skip serial benchmarking and dispatch tasks directly to parallel workers.
#' @param checkpoint_dir Optional directory path to save checkpoint files.
#'   Finished splits/tasks are loaded from disk on rerun when their fingerprint
#'   matches, skipping recomputation.
#' @param checkpoint_mismatch Action when an existing checkpoint file's
#'   fingerprint does not match the current inputs/settings: `"recompute"`
#'   (default; warns and recomputes), `"overwrite"` (overwrites mismatched files
#'   in place and removes stale checkpoints from previous runs), or `"error"`
#'   (stops immediately).
#' @param shard Optional integer vector `c(i, n)` specifying that this call
#'   should process shard `i` of `n` across machines into `checkpoint_dir`.
#'   Use [collect_checkpoints()] to merge all shards once finished.
#' @param chunk_size Optional positive integer passed to
#'   [furrr::furrr_options()] controlling the number of tasks batched per worker
#'   invocation. Defaults to `NULL` (one chunk per worker so each worker
#'   receives all of its assigned tasks in a single batch).
#' @param on_complete Optional callback `function(result)` (or option
#'   `templecbe.on_complete`) invoked when the run finishes or errors.
#' @param .fn_name Internal caller function name for logging and checkpointing metadata.
#' @return A tibble of [glmnet_IBS()] results with an `inner_resamples_splits`
#'   column giving the split's identifier.
#' @seealso [glmnet_IBS()], [tune_over_alpha()], [collect_checkpoints()],
#'   [clean_checkpoints()], [nested_cv_coxnet()]
#' @export
tune_alpha_by_split <- function(object,
                                ...,
                                num_alpha_values = 10,
                                num_fixed = 6,
                                alphas = NULL,
                                formulas = NULL,
                                progress = FALSE,
                                seed = NULL,
                                calibrate = TRUE,
                                checkpoint_dir = NULL,
                                checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                shard = NULL,
                                chunk_size = NULL,
                                on_complete = getOption("templecbe.on_complete", NULL),
                                .fn_name = "tune_alpha_by_split") {
  run_with_on_complete(on_complete = on_complete, fn = .fn_name, expr = {
    require_packages("furrr", "tune_alpha_by_split")
    checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)
    shard <- validate_shard_spec(shard)
    if (!is.null(chunk_size)) {
      chunk_size <- as.integer(chunk_size)
      if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
        stop("`chunk_size` must be NULL or a positive integer.", call. = FALSE)
      }
    }
    dots <- list(...)

    if (is.null(object$splits) || length(object$splits) == 0L) {
      stop("`object` must contain a non-empty `splits` list.", call. = FALSE)
    }
    n_splits <- length(object$splits)

    if (!is.null(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(progress)) {
        existing_ckpts <- list.files(checkpoint_dir, pattern = "^tune_split_.*\\.rds$")
        if (length(existing_ckpts) > 0) {
          message(sprintf("Found %d cached checkpoint file(s) in '%s'; skipping.", length(existing_ckpts), checkpoint_dir))
        }
      }
    }

    if (!is.null(formulas)) {
      if ("formula" %in% names(dots)) {
        stop("Pass either `formulas` or `formula`, not both.", call. = FALSE)
      }
      if (inherits(formulas, "formula")) {
        formulas <- list(formulas)
      }
      formulas <- vapply(formulas, formula_terms_string, character(1), USE.NAMES = FALSE)
      if (!is.null(alphas) && length(alphas) != length(formulas)) {
        stop("`alphas` must have one value per element of `formulas`.", call. = FALSE)
      }
    }

    if (!is.null(seed)) {
      local_seed(seed)
      split_seeds <- as.list(sample.int(1e9, n_splits))
    } else {
      split_seeds <- vector("list", n_splits)
    }

    split_names <- names(object$splits)
    if (is.null(split_names) || !all(nzchar(split_names))) {
      split_names <- as.character(seq_len(n_splits))
    }

    # Pre-generate deterministic per-split alpha/formula grids and per-task seeds
    split_grids <- lapply(seq_len(n_splits), function(split_idx) {
      s_seed <- split_seeds[[split_idx]]
      s_id <- split_names[split_idx]
      gen_one <- function() {
        if (!is.null(formulas)) {
          a_vec <- if (is.null(alphas)) stats::runif(length(formulas)) else as.numeric(alphas)
          f_vec <- formulas
        } else {
          a_vec <- if (is.null(alphas)) alpha_grid(num_alpha_values, num_fixed) else sort(as.numeric(alphas))
          f_vec <- rep(NA_character_, length(a_vec))
        }
        t_seeds <- if (!is.null(s_seed)) sample.int(1e9, length(a_vec)) else rep(NA_integer_, length(a_vec))
        tibble::tibble(
          split_idx = split_idx,
          split_id = s_id,
          item_idx = seq_along(a_vec),
          n_items = length(a_vec),
          alpha = a_vec,
          formula = f_vec,
          split_seed = if (is.null(s_seed)) NA_integer_ else as.integer(s_seed),
          fit_seed = as.integer(t_seeds)
        )
      }
      if (!is.null(s_seed)) {
        with_local_seed(s_seed, gen_one())
      } else {
        gen_one()
      }
    })

    tasks <- dplyr::bind_rows(split_grids)
    tasks$task_idx <- seq_len(nrow(tasks))
    n_tasks <- nrow(tasks)

    # Compute run fingerprint, split fingerprints, and task fingerprints
    splits_canon <- lapply(object$splits, canonicalize_split_for_hash)
    run_fp <- compute_checkpoint_fingerprint(
      split = NULL,
      dots = dots,
      seed = seed,
      extra = list(
        fn = "summarize_tune_results",
        splits = splits_canon,
        num_alpha_values = num_alpha_values,
        num_fixed = num_fixed,
        alphas = if (!is.null(alphas)) round(as.numeric(alphas), 12) else NULL,
        formulas = formulas
      )
    )

    split_fps <- lapply(seq_len(n_splits), function(split_idx) {
      sg <- split_grids[[split_idx]]
      compute_checkpoint_fingerprint(
        split = object$splits[[split_idx]],
        dots = dots,
        seed = if (is.na(sg$split_seed[1])) NULL else sg$split_seed[1],
        extra = list(
          fn = "summarize_tune_results_split",
          alphas = round(sg$alpha, 12),
          formulas = sg$formula
        )
      )
    })

    task_fps <- lapply(seq_len(n_tasks), function(k) {
      row <- tasks[k, ]
      compute_checkpoint_fingerprint(
        split = object$splits[[row$split_idx]],
        dots = dots,
        alpha = row$alpha,
        formula = if (is.na(row$formula)) NULL else row$formula,
        seed = if (is.na(row$fit_seed)) NULL else row$fit_seed,
        extra = list(fn = "summarize_tune_results_task", split_id = row$split_id, item_idx = row$item_idx)
      )
    })

    # Verify existing split & task checkpoints up front for active splits/tasks
    if (!is.null(shard)) {
      active_mask <- ((seq_len(n_tasks) - 1L) %% shard[2]) + 1L == shard[1]
    } else {
      active_mask <- rep(TRUE, n_tasks)
    }
    active_indices <- which(active_mask)
    active_splits <- unique(tasks$split_idx[active_indices])

    split_cache <- vector("list", n_splits)
    mismatch_msgs <- character()

    if (!is.null(checkpoint_dir)) {
      for (sp_idx in active_splits) {
        safe_id <- gsub("[^A-Za-z0-9._-]", "_", split_names[sp_idx])
        sp_file <- file.path(checkpoint_dir, sprintf("tune_split_%s.rds", safe_id))
        if (file.exists(sp_file)) {
          chk <- read_verified_checkpoint(sp_file, split_fps[[sp_idx]], checkpoint_mismatch = checkpoint_mismatch)
          if (identical(chk$status, "hit")) {
            split_cache[[sp_idx]] <- chk$value
          } else if (identical(chk$status, "mismatch")) {
            mismatch_msgs <- c(mismatch_msgs, chk$message)
            unlink(sp_file)
          }
        }
      }
      for (k in active_indices) {
        row <- tasks[k, ]
        safe_id <- gsub("[^A-Za-z0-9._-]", "_", row$split_id)
        tk_file <- file.path(checkpoint_dir, sprintf("tune_task_split_%s_item_%03d.rds", safe_id, row$item_idx))
        if (file.exists(tk_file)) {
          chk <- read_verified_checkpoint(tk_file, task_fps[[k]], checkpoint_mismatch = checkpoint_mismatch)
          if (identical(chk$status, "mismatch")) {
            mismatch_msgs <- c(mismatch_msgs, chk$message)
            unlink(tk_file)
          }
        }
      }
      if (length(mismatch_msgs) > 0L && checkpoint_mismatch != "overwrite") {
        warning(sprintf(
          "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed. First mismatch: %s",
          length(mismatch_msgs), checkpoint_dir, mismatch_msgs[1]
        ), call. = FALSE)
      }
    }

    # Build shared worker bundle once so data, recipe, and splits are not serialized per task (D1)
    shared_wb <- create_shared_worker_bundle(object$splits, dots, run_fp)
    on.exit(unlink(shared_wb$bundle_file), add = TRUE)

    is_parallel_plan <- requireNamespace("future", quietly = TRUE) && !inherits(future::plan(), "sequential")
    furrr_pkgs <- furrr_worker_packages()

    # Construct worker function in a minimal environment so `future` does not
    # serialize the parent frame (`object`, `dots`, `splits_canon`, etc.)
    bundle_file <- shared_wb$bundle_file
    fallback_payload <- if (is_parallel_plan && !file.exists(bundle_file)) shared_wb$payload else NULL
    worker_env <- new.env(parent = asNamespace("TempleCBE"))
    worker_env$tasks <- tasks
    worker_env$task_fps <- task_fps
    worker_env$split_cache <- split_cache
    worker_env$checkpoint_dir <- checkpoint_dir
    worker_env$run_fp <- run_fp
    worker_env$n_tasks <- n_tasks
    worker_env$shard <- shard
    worker_env$bundle_file <- bundle_file
    worker_env$fallback_payload <- fallback_payload
    worker_env$is_parallel_plan <- is_parallel_plan

    run_one_flat_task <- function(k) {
      if (is_parallel_plan && !isTRUE(getOption("templecbe_worker_blas_configured", FALSE))) {
        Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
        options(templecbe_worker_blas_configured = TRUE)
      }

      row <- tasks[k, ]
      sp_idx <- row$split_idx
      s_id <- row$split_id
      item_idx <- row$item_idx
      a <- row$alpha
      f <- if (is.na(row$formula)) NULL else row$formula
      t_fp <- task_fps[[k]]

      # 1. Check if full split checkpoint was already verified and loaded
      if (!is.null(split_cache[[sp_idx]])) {
        sp_df <- split_cache[[sp_idx]]
        sub_df <- sp_df[abs(sp_df$alpha - a) < 1e-10, , drop = FALSE]
        if (!is.null(f) && "formula" %in% names(sub_df)) {
          sub_df <- sub_df[sub_df$formula == f, , drop = FALSE]
        }
        if (nrow(sub_df) > 0L) {
          return(list(
            task_idx = k, split_idx = sp_idx, split_id = s_id, item_idx = item_idx,
            ok = !all(is.na(sub_df$IBS)), error_msg = NULL, result = sub_df
          ))
        }
      }

      # 2. Check per-task checkpoint file
      tk_file <- if (!is.null(checkpoint_dir)) {
        safe_id <- gsub("[^A-Za-z0-9._-]", "_", s_id)
        file.path(checkpoint_dir, sprintf("tune_task_split_%s_item_%03d.rds", safe_id, item_idx))
      } else {
        NULL
      }
      if (!is.null(tk_file) && file.exists(tk_file)) {
        chk <- read_verified_checkpoint(tk_file, t_fp, checkpoint_mismatch = "recompute")
        if (identical(chk$status, "hit")) {
          val <- chk$value
          err_reason <- attr(val, "failure_reason", exact = TRUE)
          is_ok <- is.data.frame(val) && nrow(val) > 0L && !all(is.na(val$IBS))
          return(list(
            task_idx = k, split_idx = sp_idx, split_id = s_id, item_idx = item_idx,
            ok = is_ok, error_msg = err_reason, result = val
          ))
        }
      }

      # 3. Load shared bundle once per worker & pre-prep split once per worker
      bundle <- get_worker_bundle(run_fp, bundle_file = bundle_file, fallback_payload = fallback_payload)
      prep_sp <- get_worker_prepared_split(bundle, sp_idx)

      if (!is.na(row$fit_seed)) {
        local_seed(row$fit_seed)
      }
      call_args <- c(
        list(object = bundle$splits[[sp_idx]], alpha = a, .prepared_split = prep_sp),
        bundle$dots
      )
      if (!is.null(f)) {
        call_args$formula <- f
      }

      fit_warns <- character()
      fit_err <- NULL
      t_fit_start <- proc.time()["elapsed"]
      res <- tryCatch(
        withCallingHandlers(
          do.call(glmnet_IBS, call_args),
          warning = function(w) {
            msg <- conditionMessage(w)
            if (grepl("glmnet_IBS\\(\\) could not fit", msg)) {
              fit_warns <<- c(fit_warns, msg)
              invokeRestart("muffleWarning")
            }
          },
          message = function(m) {
            if (grepl("no censored subjects", conditionMessage(m))) {
              invokeRestart("muffleMessage")
            }
          }
        ),
        error = function(e) {
          fit_err <<- conditionMessage(e)
          NULL
        }
      )

      if (!is.null(res) && !is.null(f)) {
        res$formula <- f
      }

      err_reason <- fit_err
      if (is.null(err_reason) && !is.null(res)) {
        err_reason <- attr(res, "failure_reason", exact = TRUE)
      }
      if (is.null(err_reason) && length(fit_warns) > 0L) {
        err_reason <- fit_warns[1]
      }

      is_ok <- is.data.frame(res) && nrow(res) > 0L && !all(is.na(res$IBS))
      if (is_ok && !is.null(tk_file)) {
        res_to_save <- res
        res_to_save$inner_resamples_splits <- s_id
        write_verified_checkpoint(
          res_to_save, tk_file, fingerprint = t_fp, run_fingerprint = run_fp,
          meta = list(
            fn = "summarize_tune_results",
            task_idx = k, n_tasks = n_tasks,
            split_idx = sp_idx, split_id = s_id,
            item_idx = item_idx, n_items = row$n_items,
            alpha = a, formula = f, shard = shard
          )
        )
      }

      t_fit_dur <- as.numeric(proc.time()["elapsed"] - t_fit_start)
      list(
        task_idx = k, split_idx = sp_idx, split_id = s_id, item_idx = item_idx,
        ok = is_ok, error_msg = err_reason, result = res, fit_dur = t_fit_dur
      )
    }
    environment(run_one_flat_task) <- worker_env

    f_opts <- if (!is.null(chunk_size)) {
      furrr::furrr_options(seed = TRUE, packages = furrr_pkgs, chunk_size = chunk_size)
    } else {
      furrr::furrr_options(seed = TRUE, packages = furrr_pkgs)
    }

    n_workers <- if (requireNamespace("future", quietly = TRUE) && !inherits(future::plan(), "sequential")) {
      future::nbrOfWorkers()
    } else {
      1L
    }
    physical_cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
    logical_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)

    t_start <- proc.time()["elapsed"]
    t_calib_dur <- 0.0

    if (isTRUE(calibrate) && n_workers > 1L && length(active_indices) > 1L) {
      t_calib_start <- proc.time()["elapsed"]

      # 1. Warm-up task serially (loads bundle, models, packages; no progress output)
      warmup_idx <- active_indices[1]
      warmup_out <- run_one_flat_task(warmup_idx)

      # 2. Benchmark on a concise sample of tasks run serially after warm-up
      n_bench <- min(2L, length(active_indices) - 1L)
      bench_indices <- if (n_bench > 0L) active_indices[seq(2L, length.out = n_bench)] else integer()
      bench_outs <- vector("list", n_bench)
      bench_durs <- numeric(n_bench)

      for (b_i in seq_len(n_bench)) {
        t_b_start <- proc.time()["elapsed"]
        b_res <- run_one_flat_task(bench_indices[b_i])
        b_dur <- as.numeric(proc.time()["elapsed"] - t_b_start)
        bench_durs[b_i] <- max(0.001, b_res$fit_dur %||% b_dur)
        bench_outs[[b_i]] <- b_res
      }

      t_calib_dur <- as.numeric(proc.time()["elapsed"] - t_calib_start)

      sample_durs <- if (n_bench > 0L) bench_durs else warmup_out$fit_dur %||% 0.001
      ser_task_dur <- max(0.001, stats::median(sample_durs))
      ser_task_dur_min <- max(0.001, min(sample_durs))
      ser_task_dur_max <- max(0.001, max(sample_durs))
      warmup_out$fit_dur <- warmup_out$fit_dur %||% ser_task_dur

      # 3. Dispatch remaining tasks to parallel workers
      n_done_serial <- 1L + n_bench
      rem_indices <- if (length(active_indices) > n_done_serial) {
        active_indices[-seq_len(n_done_serial)]
      } else {
        integer()
      }

      rem_outs <- if (length(rem_indices) > 0L) {
        furrr::future_map(
          rem_indices,
          run_one_flat_task,
          .options = f_opts,
          .progress = progress
        )
      } else {
        list()
      }

      task_outcomes <- c(list(warmup_out), bench_outs, rem_outs)
    } else {
      task_outcomes <- furrr::future_map(
        active_indices,
        run_one_flat_task,
        .options = f_opts,
        .progress = progress
      )
      all_durs <- vapply(task_outcomes, function(x) x$fit_dur %||% 0.001, numeric(1))
      ser_task_dur <- if (length(all_durs) > 0L) stats::median(all_durs) else 0.001
      ser_task_dur_min <- if (length(all_durs) > 0L) min(all_durs) else 0.001
      ser_task_dur_max <- if (length(all_durs) > 0L) max(all_durs) else 0.001
    }
    t_elapsed <- as.numeric(proc.time()["elapsed"] - t_start)

    n_active <- length(active_indices)
    serial_baseline_total <- ser_task_dur * n_active
    serial_baseline_min <- ser_task_dur_min * n_active
    serial_baseline_max <- ser_task_dur_max * n_active

    real_speedup <- if (t_elapsed > 0) serial_baseline_total / t_elapsed else 1.0
    speedup_min <- if (t_elapsed > 0) serial_baseline_min / t_elapsed else 1.0
    speedup_max <- if (t_elapsed > 0) serial_baseline_max / t_elapsed else 1.0

    worker_cpu_total <- sum(vapply(task_outcomes, function(x) x$fit_dur %||% 0, numeric(1)))
    worker_avg_sec <- worker_cpu_total / max(1L, n_active)
    worker_concurrency_slowdown <- if (ser_task_dur > 0) worker_avg_sec / ser_task_dur else 1.0
    ideal_speedup <- if (t_elapsed > 0) worker_cpu_total / t_elapsed else 1.0
    ipc_overhead <- max(0, t_elapsed - (worker_cpu_total / max(1L, n_workers)))

    timing_breakdown <- list(
      n_tasks = n_active,
      n_splits = length(active_splits),
      n_workers = n_workers,
      physical_cores = physical_cores,
      logical_cores = logical_cores,
      elapsed_sec = t_elapsed,
      calibrated = isTRUE(calibrate),
      calibration_sec = t_calib_dur,
      measured_serial_task_sec = ser_task_dur,
      measured_serial_task_range_sec = c(ser_task_dur_min, ser_task_dur_max),
      measured_serial_baseline_sec = serial_baseline_total,
      measured_serial_baseline_range_sec = c(serial_baseline_min, serial_baseline_max),
      real_speedup = real_speedup,
      speedup = real_speedup,
      speedup_range = c(speedup_min, speedup_max),
      worker_avg_task_sec = worker_avg_sec,
      avg_task_sec = worker_avg_sec,
      worker_concurrency_slowdown = worker_concurrency_slowdown,
      total_worker_cpu_sec = worker_cpu_total,
      total_cpu_sec = worker_cpu_total,
      ideal_speedup = ideal_speedup,
      ipc_overhead_sec = ipc_overhead
    )

    if (isTRUE(progress)) {
      calib_msg <- if (t_calib_dur > 0) sprintf("; calibration took %.2f s", t_calib_dur) else ""
      msg <- sprintf(
        "tune_alpha_by_split() timing breakdown:\n  - Completed %d task(s) across %d split(s) in %.2f s (%.2fx [range: %.2fx - %.2fx] real speedup on %d workers vs %.2f s [range: %.2f - %.2f s] measured serial baseline%s)\n  - Per-task fit time: %.2f s serial baseline vs %.2f s in workers (%.2fx worker concurrency slowdown)\n  - Parallel IPC / serialization / sync overhead: %.2f s",
        n_active, length(active_splits), t_elapsed, real_speedup, speedup_min, speedup_max, n_workers,
        serial_baseline_total, serial_baseline_min, serial_baseline_max, calib_msg,
        ser_task_dur, worker_avg_sec, worker_concurrency_slowdown, ipc_overhead
      )
      if (!is.na(physical_cores) && n_workers > physical_cores) {
        msg <- paste0(
          msg, sprintf(
            "\n  - NOTE: Worker count (%d) exceeds physical CPU cores (%d). Hyper-threaded logical cores share execution units and cache, which often doubles per-task compute time in concurrent workers. Recommend setting workers <= %d.",
            n_workers, physical_cores, physical_cores
          )
        )
      }
      message(msg)
    }

    # Group outcomes by split to check failures, emit at most 1 warning per split, and write split checkpoints
    res_by_split <- lapply(active_splits, function(sp_idx) {
      s_id <- split_names[sp_idx]
      sp_outs <- Filter(function(x) x$split_idx == sp_idx, task_outcomes)
      ok_flags <- vapply(sp_outs, function(x) isTRUE(x$ok), logical(1))
      n_fits <- length(sp_outs)
      n_ok <- sum(ok_flags)
      if (n_ok == 0L) {
        errs <- unlist(lapply(sp_outs, `[[`, "error_msg"))
        first_err <- if (length(errs) > 0L && nzchar(errs[1])) errs[1] else "all IBS values are NA"
        stop(sprintf("All %d model fit(s) failed for split '%s'. First error: %s", n_fits, s_id, first_err), call. = FALSE)
      }
      if (n_ok < n_fits) {
        failed_outs <- sp_outs[!ok_flags]
        errs <- unlist(lapply(failed_outs, `[[`, "error_msg"))
        first_err <- if (length(errs) > 0L && nzchar(errs[1])) errs[1] else "IBS is NA"
        warning(sprintf("%d of %d model fit(s) failed for split '%s'. First error: %s", n_fits - n_ok, n_fits, s_id, first_err), call. = FALSE)
      }
      good_dfs <- lapply(sp_outs[ok_flags], function(x) {
        df <- x$result
        df$inner_resamples_splits <- NULL
        df
      })
      res_split <- dplyr::bind_rows(good_dfs)

      # Save full split checkpoint if all items of this split were evaluated
      expected_n_items <- split_grids[[sp_idx]]$n_items[1]
      if (!is.null(checkpoint_dir) && is.null(shard) && length(sp_outs) == expected_n_items) {
        safe_id <- gsub("[^A-Za-z0-9._-]", "_", s_id)
        sp_file <- file.path(checkpoint_dir, sprintf("tune_split_%s.rds", safe_id))
        write_verified_checkpoint(
          res_split, sp_file, fingerprint = split_fps[[sp_idx]], run_fingerprint = run_fp,
          meta = list(
            fn = "tune_alpha_by_split",
            split_idx = sp_idx, split_id = s_id,
            n_splits = n_splits, n_tasks = n_tasks
          )
        )
      }

      dplyr::bind_cols(
        tibble::tibble(inner_resamples_splits = rep(s_id, nrow(res_split))),
        res_split
      )
    })

    if (!is.null(checkpoint_dir) && is.null(shard) && checkpoint_mismatch == "overwrite") {
      clean_checkpoints(checkpoint_dir, keep = "current", run_fingerprint = run_fp)
    }

    res_all <- dplyr::bind_rows(res_by_split)
    if (!is.data.frame(res_all) || nrow(res_all) == 0L || all(is.na(res_all$IBS))) {
      stop("All model fits across all splits failed in tune_alpha_by_split().", call. = FALSE)
    }

    attr(res_all, "checkpoint_run_fingerprint") <- run_fp
    attr(res_all, "seed_scheme") <- current_seed_scheme()
    attr(res_all, "elapsed_seconds") <- t_elapsed
    attr(res_all, "timing_breakdown") <- timing_breakdown
    res_all
  })
}

#' @rdname tune_alpha_by_split
#' @export
summarize_tune_results <- function(object,
                                   ...,
                                   num_alpha_values = 10,
                                   num_fixed = 6,
                                   alphas = NULL,
                                   formulas = NULL,
                                   progress = FALSE,
                                   seed = NULL,
                                   calibrate = TRUE,
                                   checkpoint_dir = NULL,
                                   checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                   shard = NULL,
                                   chunk_size = NULL,
                                   on_complete = getOption("templecbe.on_complete", NULL)) {
  lifecycle::deprecate_warn("0.5.8", "summarize_tune_results()", "tune_alpha_by_split()")
  tune_alpha_by_split(
    object = object,
    ...,
    num_alpha_values = num_alpha_values,
    num_fixed = num_fixed,
    alphas = alphas,
    formulas = formulas,
    progress = progress,
    seed = seed,
    calibrate = calibrate,
    checkpoint_dir = checkpoint_dir,
    checkpoint_mismatch = checkpoint_mismatch,
    shard = shard,
    chunk_size = chunk_size,
    on_complete = on_complete,
    .fn_name = "summarize_tune_results"
  )
}

#' Run an Expression With a Temporary Random Seed
#'
#' @keywords internal
#' @noRd
with_local_seed <- function(seed, expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit( {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)
  }
  set.seed(seed)
  force(expr)
}

#' Collect and Merge Sharded Checkpoints From `checkpoint_dir`
#'
#' Merges per-task checkpoint files written by [summarize_tune_results()] or
#' [score_outer()] across multiple machines (`shard = c(i, n)`). Verifies that
#' all checkpoint files carry the same `checkpoint_run_fingerprint` and
#' `seed_scheme` so results from different data, recipes, feature sets, seed
#' schemes, or settings cannot be combined by mistake.
#'
#' @param checkpoint_dir Directory containing `.rds` checkpoint files.
#' @param type Optional checkpoint type to collect: `"auto"` (default; inferred
#'   from files in `checkpoint_dir`), `"tune"` ([summarize_tune_results()]), or
#'   `"score_outer"` ([score_outer()]).
#' @param allow_incomplete Logical (default `FALSE`). When `FALSE`, stops with
#'   an error if any tasks from the full run grid are missing.
#' @return A merged tibble matching the return format of
#'   [summarize_tune_results()] or [score_outer()].
#' @seealso [summarize_tune_results()], [score_outer()], [clean_checkpoints()]
#' @export
collect_checkpoints <- function(checkpoint_dir,
                                type = c("auto", "tune", "score_outer"),
                                allow_incomplete = FALSE) {
  type <- match.arg(type)
  if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L || !dir.exists(checkpoint_dir)) {
    stop("`checkpoint_dir` must be an existing directory path.", call. = FALSE)
  }

  all_rds_files <- sort(list.files(checkpoint_dir, pattern = "\\.rds$", full.names = TRUE))
  task_tune_files <- sort(list.files(checkpoint_dir, pattern = "^tune_task_split_.*\\.rds$", full.names = TRUE))
  split_tune_files <- sort(list.files(checkpoint_dir, pattern = "^tune_split_.*\\.rds$", full.names = TRUE))
  outer_tune_files <- sort(list.files(checkpoint_dir, pattern = "^tune_outer_.*\\.rds$", full.names = TRUE))
  score_files <- sort(list.files(checkpoint_dir, pattern = "^score_(fold|outer).*\\.rds$", full.names = TRUE))

  if (type == "auto") {
    has_tune <- (length(task_tune_files) + length(split_tune_files) + length(outer_tune_files)) > 0L
    has_score <- length(score_files) > 0L
    if (has_tune && has_score) {
      stop("`checkpoint_dir` contains both tuning and `score_outer` checkpoints; specify `type = \"tune\"` or `type = \"score_outer\"`.", call. = FALSE)
    }
    if (!has_tune && !has_score) {
      if (length(all_rds_files) == 0L) {
        stop(sprintf("No checkpoint files found in '%s'.", checkpoint_dir), call. = FALSE)
      }
      files <- all_rds_files
    } else {
      files <- if (has_tune) {
        if (length(outer_tune_files) > 0L) {
          outer_tune_files
        } else if (length(task_tune_files) > 0L) {
          task_tune_files
        } else {
          split_tune_files
        }
      } else {
        score_files
      }
    }
  } else if (type == "tune") {
    files <- if (length(outer_tune_files) > 0L) {
      outer_tune_files
    } else if (length(task_tune_files) > 0L) {
      task_tune_files
    } else {
      split_tune_files
    }
  } else {
    files <- score_files
  }

  if (length(files) == 0L) {
    stop(sprintf("No '%s' checkpoint files found in '%s'.", type, checkpoint_dir), call. = FALSE)
  }

  objs <- lapply(files, function(f) {
    obj <- tryCatch(readRDS(f), error = function(e) {
      stop(sprintf("Failed to read checkpoint file '%s': %s", basename(f), conditionMessage(e)), call. = FALSE)
    })
    obj
  })

  run_fps <- vapply(objs, function(x) {
    fp <- attr(x, "checkpoint_run_fingerprint", exact = TRUE)
    if (is.null(fp) || !nzchar(fp)) {
      fp <- attr(x, "checkpoint_fingerprint", exact = TRUE)
    }
    if (is.null(fp) || !nzchar(fp)) "<missing>" else as.character(fp)
  }, character(1))

  if (any(run_fps == "<missing>")) {
    stop("One or more checkpoint files are missing a `checkpoint_fingerprint`. Cannot safely merge unverified checkpoints.", call. = FALSE)
  }
  unique_fps <- unique(run_fps)
  if (length(unique_fps) > 1L) {
    stop(sprintf(
      "Checkpoint fingerprint mismatch across files in '%s' (%d distinct run fingerprints found). Mixed runs with different inputs or settings cannot be combined.",
      checkpoint_dir, length(unique_fps)
    ), call. = FALSE)
  }

  schemes <- vapply(objs, function(x) {
    s <- attr(x, "seed_scheme", exact = TRUE) %||%
      attr(x, "checkpoint_meta", exact = TRUE)$seed_scheme
    if (is.null(s) || !nzchar(s)) "<missing>" else as.character(s)
  }, character(1))
  unique_schemes <- unique(schemes)
  if (length(unique_schemes) > 1L) {
    stop(sprintf(
      "Checkpoint seed_scheme mismatch across files in '%s' (%s). Checkpoints from different seed schemes cannot be combined.",
      checkpoint_dir, paste(unique_schemes, collapse = " vs ")
    ), call. = FALSE)
  }

  metas <- lapply(objs, function(x) attr(x, "checkpoint_meta", exact = TRUE))
  task_ids <- vapply(seq_along(objs), function(i) {
    m <- metas[[i]]
    if (!is.null(m$task_idx)) as.integer(m$task_idx) else i
  }, integer(1))
  n_expected <- metas[[1]]$n_tasks

  if (!is.null(n_expected) && !is.na(n_expected)) {
    missing_tasks <- setdiff(seq_len(as.integer(n_expected)), task_ids)
    if (length(missing_tasks) > 0L) {
      msg <- sprintf(
        "Checkpoint collection is incomplete: %d of %d task(s) missing (%s).",
        length(missing_tasks), as.integer(n_expected),
        paste(utils::head(missing_tasks, 10), collapse = ", ")
      )
      if (!isTRUE(allow_incomplete)) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }

  ord <- order(task_ids)
  objs <- objs[ord]
  metas <- metas[ord]

  if (type == "tune") {
    objs <- Map(function(df, m) {
      if (!"inner_resamples_splits" %in% names(df)) {
        s_id <- if (!is.null(m$split_id)) as.character(m$split_id) else "1"
        df <- dplyr::bind_cols(tibble::tibble(inner_resamples_splits = rep(s_id, nrow(df))), df)
      } else {
        cols <- c("inner_resamples_splits", setdiff(names(df), "inner_resamples_splits"))
        df <- df[, cols, drop = FALSE]
      }
      df
    }, objs, metas)
  }

  all_df <- all(vapply(objs, is.data.frame, logical(1)))
  merged <- if (all_df) {
    dplyr::bind_rows(objs)
  } else {
    names(objs) <- basename(files)
    objs
  }
  attr(merged, "checkpoint_run_fingerprint") <- unique_fps[1]
  attr(merged, "seed_scheme") <- if (unique_schemes[1] == "<missing>") current_seed_scheme() else unique_schemes[1]
  merged
}

#' Clean Up Mismatched or Superseded Checkpoint Files (E5)
#'
#' Inspects `.rds` checkpoint files in `dir` and removes files from older runs,
#' mismatched fingerprints, or superseded `seed_scheme` versions while keeping
#' the active run's checkpoints.
#'
#' @param dir Directory containing checkpoint `.rds` files.
#' @param keep Retention policy:
#'   \describe{
#'     \item{`"current"`}{(Default) Keeps checkpoint files belonging to the most
#'       recent `checkpoint_run_fingerprint` in each checkpoint family (or the
#'       explicitly supplied `run_fingerprint`) and removes older, mismatched,
#'       or corrupted checkpoint files.}
#'     \item{`"matching"`}{Keeps only checkpoint files matching the supplied
#'       `run_fingerprint` and/or `seed_scheme`.}
#'     \item{`"none"`}{Removes all checkpoint files in `dir`.}
#'   }
#' @param run_fingerprint Optional expected `checkpoint_run_fingerprint` hash.
#'   When `NULL` under `keep = "current"`, the fingerprint of the newest valid
#'   checkpoint file per family (`tune` / `score_outer`) is used.
#' @param seed_scheme Optional expected seed scheme string (e.g. `"v0.5.6"`).
#' @param recursive Logical (default `FALSE`). Search subdirectories of `dir`.
#' @param dry_run Logical (default `FALSE`). When `TRUE`, reports which files
#'   would be kept or removed without deleting anything from disk.
#' @return A tibble of class `cbe_clean_checkpoints` with columns `file`, `path`,
#'   `family`, `run_fingerprint`, `seed_scheme`, `mtime`, `action` (`"kept"` or
#'   `"removed"`), and `reason`, plus attributes `n_kept` and `n_removed`.
#' @seealso [collect_checkpoints()], [summarize_tune_results()], [score_outer()]
#' @export
clean_checkpoints <- function(dir,
                              keep = c("current", "matching", "none"),
                              run_fingerprint = NULL,
                              seed_scheme = NULL,
                              recursive = FALSE,
                              dry_run = FALSE) {
  keep <- match.arg(keep)
  if (!is.character(dir) || length(dir) != 1L || !dir.exists(dir)) {
    stop("`dir` must be an existing directory path.", call. = FALSE)
  }
  if (keep == "matching" && is.null(run_fingerprint) && is.null(seed_scheme)) {
    stop("Specify `run_fingerprint` and/or `seed_scheme` when `keep = \"matching\"`.", call. = FALSE)
  }

  ckpt_pattern <- "^(tune_alpha_|tune_split_|tune_task_split_|tune_outer_|score_fold_|score_outer|ckpt_|fit_|cv_|nested_|impute_|missforest_|missranger_).*\\.rds$"
  all_rds <- list.files(dir, pattern = "\\.rds$", full.names = TRUE, recursive = recursive)
  if (length(all_rds) == 0L) {
    empty_tbl <- tibble::tibble(
      file = character(),
      path = character(),
      family = character(),
      run_fingerprint = character(),
      seed_scheme = character(),
      mtime = as.POSIXct(character()),
      action = character(),
      reason = character()
    )
    attr(empty_tbl, "n_kept") <- 0L
    attr(empty_tbl, "n_removed") <- 0L
    class(empty_tbl) <- c("cbe_clean_checkpoints", class(empty_tbl))
    return(empty_tbl)
  }

  rows <- lapply(all_rds, function(f) {
    bn <- basename(f)
    is_named_ckpt <- grepl(ckpt_pattern, bn)
    f_info <- file.info(f)
    mtime <- f_info$mtime
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    is_envelope <- inherits(obj, "templecbe_checkpoint_envelope") ||
      (is.list(obj) && !is.data.frame(obj) && "checkpoint_fingerprint" %in% names(obj))
    has_fp_attr <- !is.null(obj) && (
      is_envelope ||
      !is.null(attr(obj, "checkpoint_fingerprint", exact = TRUE)) ||
      !is.null(attr(obj, "checkpoint_run_fingerprint", exact = TRUE)) ||
      !is.null(attr(obj, "checkpoint_meta", exact = TRUE))
    )
    if (!is_named_ckpt && !has_fp_attr) {
      return(NULL)
    }
    family <- if (grepl("^score_fold_", bn)) {
      "score_outer"
    } else if (grepl("^tune_", bn)) {
      "tune"
    } else if (is_envelope) {
      obj$checkpoint_meta$fn %||% "checkpoint"
    } else {
      attr(obj, "checkpoint_meta", exact = TRUE)$fn %||% "checkpoint"
    }
    inner_val <- if (is_envelope) obj$value else obj
    valid_obj <- is.data.frame(inner_val) && nrow(inner_val) > 0L
    run_fp <- if (is_envelope) {
      obj$checkpoint_run_fingerprint %||% obj$checkpoint_fingerprint %||% NA_character_
    } else if (!is.null(obj)) {
      attr(obj, "checkpoint_run_fingerprint", exact = TRUE) %||%
        attr(obj, "checkpoint_fingerprint", exact = TRUE) %||%
        NA_character_
    } else {
      NA_character_
    }
    scheme <- if (is_envelope) {
      obj$seed_scheme %||% obj$checkpoint_meta$seed_scheme %||% NA_character_
    } else if (!is.null(obj)) {
      attr(obj, "seed_scheme", exact = TRUE) %||%
        attr(obj, "checkpoint_meta", exact = TRUE)$seed_scheme %||%
        NA_character_
    } else {
      NA_character_
    }
    tibble::tibble(
      file = bn,
      path = normalizePath(f, winslash = "/", mustWork = FALSE),
      family = as.character(family),
      run_fingerprint = as.character(run_fp),
      seed_scheme = as.character(scheme),
      mtime = mtime,
      valid_obj = valid_obj
    )
  })

  df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (nrow(df) == 0L) {
    empty_tbl <- tibble::tibble(
      file = character(),
      path = character(),
      family = character(),
      run_fingerprint = character(),
      seed_scheme = character(),
      mtime = as.POSIXct(character()),
      action = character(),
      reason = character()
    )
    attr(empty_tbl, "n_kept") <- 0L
    attr(empty_tbl, "n_removed") <- 0L
    class(empty_tbl) <- c("cbe_clean_checkpoints", class(empty_tbl))
    return(empty_tbl)
  }

  actions <- rep("kept", nrow(df))
  reasons <- rep("current", nrow(df))

  if (keep == "none") {
    actions[] <- "removed"
    reasons[] <- "keep_none"
  } else {
    # Mark unreadable or unfingerprinted files for removal
    bad_mask <- !df$valid_obj | is.na(df$run_fingerprint) | !nzchar(df$run_fingerprint)
    actions[bad_mask] <- "removed"
    reasons[bad_mask] <- "invalid_or_missing_fingerprint"

    if (!is.null(seed_scheme)) {
      scheme_mismatch <- !bad_mask & (is.na(df$seed_scheme) | df$seed_scheme != as.character(seed_scheme))
      actions[scheme_mismatch] <- "removed"
      reasons[scheme_mismatch] <- "mismatched_seed_scheme"
    }

    if (!is.null(run_fingerprint)) {
      fp_mismatch <- (actions == "kept") & (df$run_fingerprint != as.character(run_fingerprint))
      actions[fp_mismatch] <- "removed"
      reasons[fp_mismatch] <- "mismatched_run_fingerprint"
    } else if (keep == "current") {
      # Group by directory + family to determine the newest run_fingerprint
      grp <- paste(dirname(df$path), df$family, sep = "::")
      for (g in unique(grp)) {
        idx <- which(grp == g & actions == "kept")
        if (length(idx) > 0L) {
          newest_idx <- idx[which.max(df$mtime[idx])]
          curr_fp <- df$run_fingerprint[newest_idx]
          old_idx <- idx[df$run_fingerprint[idx] != curr_fp]
          if (length(old_idx) > 0L) {
            actions[old_idx] <- "removed"
            reasons[old_idx] <- "mismatched_run_fingerprint"
          }
        }
      }
    }
  }

  df$valid_obj <- NULL
  df$action <- actions
  df$reason <- reasons

  to_remove <- df$path[df$action == "removed"]
  if (!isTRUE(dry_run) && length(to_remove) > 0L) {
    for (rf in to_remove) {
      if (file.exists(rf)) unlink(rf)
    }
  }

  attr(df, "n_kept") <- sum(df$action == "kept")
  attr(df, "n_removed") <- sum(df$action == "removed")
  attr(df, "dry_run") <- isTRUE(dry_run)
  class(df) <- c("cbe_clean_checkpoints", class(df))
  df
}

#' @export
print.cbe_clean_checkpoints <- function(x, ...) {
  cat("TempleCBE Checkpoint Cleanup\n")
  cat("----------------------------\n")
  cat(sprintf("Kept:    %d file(s)\n", attr(x, "n_kept") %||% sum(x$action == "kept")))
  cat(sprintf("Removed: %d file(s)%s\n",
              attr(x, "n_removed") %||% sum(x$action == "removed"),
              if (isTRUE(attr(x, "dry_run"))) " (dry run)" else ""))
  invisible(x)
}

#' Estimate Runtime Before a Long Tuning or Scoring Run
#'
#' Times a small sample of model fits (`n_sample`) and projects the total
#' wall-clock time for the full grid and per machine.
#'
#' @param fn Function to benchmark, such as [summarize_tune_results],
#'   [score_outer], [tune_over_alpha], or [glmnet_IBS] (or its character name).
#' @param args Named list of arguments to pass to `fn`.
#' @param n_sample Number of sample fits to time (default `3`).
#' @param workers Number of parallel workers per machine. Defaults to
#'   [future::nbrOfWorkers()] when \pkg{future} is installed, or `1L`.
#' @param n_machines Number of machines used for sharding (default `1L`, or
#'   inferred from `args$shard[2]` when present).
#' @param n_outer_folds Optional multiplier for outer CV folds when `args`
#'   represents a single outer fold (default `1L`).
#' @param n_domains Optional multiplier for domains when `args` represents a
#'   single domain (default `1L`).
#' @return A tibble of class `cbe_runtime_estimate` with per-fit timing
#'   statistics (`mean_sec_per_fit`, `median_sec_per_fit`, `sd_sec_per_fit`),
#'   grid size (`n_tasks`, `n_total_fits`), and projected wall-clock times
#'   (`projected_sequential_sec`, `projected_total_sec`,
#'   `projected_per_machine_sec`, `projected_total_mins`,
#'   `projected_per_machine_mins`).
#' @seealso [summarize_tune_results()], [score_outer()]
#' @export
estimate_runtime <- function(fn,
                             args,
                             n_sample = 3,
                             workers = NULL,
                             n_machines = NULL,
                             n_outer_folds = 1L,
                             n_domains = 1L) {
  if (is.character(fn) && length(fn) == 1L) {
    fn_name <- fn
    fn <- match.fun(fn)
  } else if (is.function(fn)) {
    fn_name <- deparse(substitute(fn))[1]
  } else {
    stop("`fn` must be a function or function name.", call. = FALSE)
  }
  if (!is.list(args)) {
    stop("`args` must be a named list of arguments for `fn`.", call. = FALSE)
  }
  n_sample <- as.integer(n_sample)
  if (is.na(n_sample) || n_sample < 1L) {
    stop("`n_sample` must be a positive integer.", call. = FALSE)
  }

  if (is.null(workers)) {
    workers <- if (requireNamespace("future", quietly = TRUE)) {
      max(1L, as.integer(future::nbrOfWorkers()))
    } else {
      1L
    }
  } else {
    workers <- max(1L, as.integer(workers))
  }

  if (is.null(n_machines)) {
    if (!is.null(args$shard) && length(args$shard) == 2L) {
      n_machines <- max(1L, as.integer(args$shard[2]))
    } else {
      n_machines <- 1L
    }
  } else {
    n_machines <- max(1L, as.integer(n_machines))
  }

  n_outer_folds <- max(1L, as.integer(n_outer_folds))
  n_domains <- max(1L, as.integer(n_domains))

  # Determine total tasks in `args` and build sample calls
  is_summarize <- identical(fn, summarize_tune_results) || identical(fn_name, "summarize_tune_results")
  is_score <- identical(fn, score_outer) || identical(fn_name, "score_outer")
  is_tune_alpha <- identical(fn, tune_over_alpha) || identical(fn_name, "tune_over_alpha")

  sample_calls <- list()
  n_tasks <- 1L

  if (is_summarize) {
    obj <- args$object %||% args[[1]]
    splits <- obj$splits
    n_splits <- length(splits)
    if (!is.null(args$formulas)) {
      f_vec <- if (inherits(args$formulas, "formula")) list(args$formulas) else args$formulas
      f_vec <- vapply(f_vec, formula_terms_string, character(1), USE.NAMES = FALSE)
      a_vec <- if (!is.null(args$alphas)) as.numeric(args$alphas) else seq(0.1, 0.9, length.out = length(f_vec))
    } else if (!is.null(args$alphas)) {
      a_vec <- as.numeric(args$alphas)
      f_vec <- rep(NA_character_, length(a_vec))
    } else {
      n_a <- args$num_alpha_values %||% 10L
      n_f <- args$num_fixed %||% min(6L, n_a)
      a_vec <- alpha_grid(n_a, n_f)
      f_vec <- rep(NA_character_, length(a_vec))
    }
    n_tasks <- n_splits * length(a_vec)
    grid_idx <- expand.grid(item_idx = seq_along(a_vec), split_idx = seq_len(n_splits))
    n_run <- min(n_sample, nrow(grid_idx))
    base_dots <- args[setdiff(names(args), c("object", "", "num_alpha_values", "num_fixed", "alphas", "formulas", "progress", "seed", "checkpoint_dir", "checkpoint_mismatch", "shard", "chunk_size", "on_complete"))]
    for (i in seq_len(n_run)) {
      sp <- splits[[grid_idx$split_idx[i]]]
      a <- a_vec[grid_idx$item_idx[i]]
      f <- f_vec[grid_idx$item_idx[i]]
      c_args <- c(list(object = sp, alpha = a), base_dots)
      if (!is.na(f)) c_args$formula <- f
      sample_calls[[i]] <- list(fn = glmnet_IBS, args = c_args)
    }
  } else if (is_score) {
    obj <- args$object %||% args[[1]]
    grid_df <- args$grid %||% args[[2]]
    splits <- if (inherits(obj, "rsplit")) {
      list(obj)
    } else if (!is.null(obj$splits)) {
      obj$splits
    } else {
      obj
    }
    n_tasks <- length(splits) * nrow(grid_df)
    grid_idx <- expand.grid(row_idx = seq_len(nrow(grid_df)), split_idx = seq_along(splits))
    n_run <- min(n_sample, nrow(grid_idx))
    base_args <- args
    base_args$checkpoint_dir <- NULL
    base_args$progress <- FALSE
    base_args$shard <- NULL
    base_args$chunk_size <- NULL
    base_args$on_complete <- NULL
    for (i in seq_len(n_run)) {
      c_args <- base_args
      c_args$object <- splits[[grid_idx$split_idx[i]]]
      c_args$grid <- grid_df[grid_idx$row_idx[i], , drop = FALSE]
      sample_calls[[i]] <- list(fn = score_outer, args = c_args)
    }
  } else if (is_tune_alpha) {
    obj <- args$object %||% args[[1]]
    if (!is.null(args$formulas)) {
      f_vec <- if (inherits(args$formulas, "formula")) list(args$formulas) else args$formulas
      f_vec <- vapply(f_vec, formula_terms_string, character(1), USE.NAMES = FALSE)
      a_vec <- if (!is.null(args$alphas)) as.numeric(args$alphas) else seq(0.1, 0.9, length.out = length(f_vec))
    } else if (!is.null(args$alphas)) {
      a_vec <- as.numeric(args$alphas)
      f_vec <- rep(NA_character_, length(a_vec))
    } else {
      n_a <- args$num_alpha_values %||% 10L
      n_f <- args$num_fixed %||% min(6L, n_a)
      a_vec <- alpha_grid(n_a, n_f)
      f_vec <- rep(NA_character_, length(a_vec))
    }
    n_tasks <- length(a_vec)
    n_run <- min(n_sample, n_tasks)
    base_dots <- args[setdiff(names(args), c("object", "", "num_alpha_values", "num_fixed", "alphas", "formulas", "progress", "seed", "checkpoint_dir", "checkpoint_mismatch", "on_complete"))]
    for (i in seq_len(n_run)) {
      c_args <- c(list(object = obj, alpha = a_vec[i]), base_dots)
      if (!is.na(f_vec[i])) c_args$formula <- f_vec[i]
      sample_calls[[i]] <- list(fn = glmnet_IBS, args = c_args)
    }
  } else {
    n_tasks <- n_sample
    for (i in seq_len(n_sample)) {
      sample_calls[[i]] <- list(fn = fn, args = args)
    }
  }

  sample_times <- vapply(sample_calls, function(sc) {
    t0 <- proc.time()[["elapsed"]]
    suppressWarnings(tryCatch(do.call(sc$fn, sc$args), error = function(e) NULL))
    max(0, proc.time()[["elapsed"]] - t0)
  }, numeric(1))

  mean_sec <- mean(sample_times)
  med_sec <- stats::median(sample_times)
  sd_sec <- if (length(sample_times) > 1L) stats::sd(sample_times) else NA_real_

  n_total_fits <- as.integer(n_tasks * n_outer_folds * n_domains)
  seq_sec <- mean_sec * n_total_fits
  total_sec <- seq_sec / workers
  per_machine_sec <- total_sec / n_machines
  physical_cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  logical_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)

  out <- tibble::tibble(
    fn = fn_name,
    n_sample = length(sample_times),
    mean_sec_per_fit = mean_sec,
    median_sec_per_fit = med_sec,
    sd_sec_per_fit = sd_sec,
    n_tasks = as.integer(n_tasks),
    n_total_fits = n_total_fits,
    workers = workers,
    physical_cores = physical_cores,
    logical_cores = logical_cores,
    n_machines = n_machines,
    projected_sequential_sec = seq_sec,
    projected_total_sec = total_sec,
    projected_per_machine_sec = per_machine_sec,
    projected_total_mins = total_sec / 60,
    projected_per_machine_mins = per_machine_sec / 60
  )
  attr(out, "sample_times") <- sample_times
  class(out) <- c("cbe_runtime_estimate", class(out))
  out
}

#' @export
print.cbe_runtime_estimate <- function(x, ...) {
  cat("TempleCBE Runtime Estimate\n")
  cat("--------------------------\n")
  cat(sprintf("Function:             %s\n", x$fn[1]))
  cat(sprintf("Sample fits timed:    %d (mean %.2f s, median %.2f s)\n",
              x$n_sample[1], x$mean_sec_per_fit[1], x$median_sec_per_fit[1]))
  cat(sprintf("Total fits in grid:   %d (tasks per call: %d)\n",
              x$n_total_fits[1], x$n_tasks[1]))
  cat(sprintf("Workers / Machines:   %d worker(s) x %d machine(s)\n",
              x$workers[1], x$n_machines[1]))
  if (!is.na(x$physical_cores[1])) {
    cat(sprintf("Recommended workers:  <= %d (physical CPU cores; hyper-threading increases per-task worker latency)\n", x$physical_cores[1]))
    if (x$workers[1] > x$physical_cores[1]) {
      cat(sprintf("  * NOTE: Current worker count (%d) exceeds physical cores (%d). Hyper-threaded logical cores share execution units and cache, which often doubles per-task compute time in concurrent workers.\n", x$workers[1], x$physical_cores[1]))
    }
  }
  cat(sprintf("Projected sequential: %.1f s (%.2f min)\n",
              x$projected_sequential_sec[1], x$projected_sequential_sec[1] / 60))
  cat(sprintf("Projected total (1m): %.1f s (%.2f min)\n",
              x$projected_total_sec[1], x$projected_total_mins[1]))
  cat(sprintf("Projected / machine:  %.1f s (%.2f min)\n",
              x$projected_per_machine_sec[1], x$projected_per_machine_mins[1]))
  invisible(x)
}


#' Turn a Formula or Formula String Into `+`-Separated Term Names
#'
#' @param formula A formula (its right-hand side is used), or a character
#'   vector of `+`-separated names, optionally starting with `~`.
#' @return A single string such as `"x1 + x2"`.
#' @keywords internal
#' @noRd
formula_terms_string <- function(formula) {
  if (inherits(formula, "formula")) {
    formula <- paste(deparse(formula[[length(formula)]]), collapse = " ")
  }
  trimws(sub("^\\s*~", "", paste0(formula, collapse = " + ")))
}

#' Build an `alpha` Grid
#'
#' @param num_alpha_values Total grid size.
#' @param num_fixed Number of evenly spaced values from 0 to 1.
#' @return A sorted numeric vector of length `num_alpha_values`.
#' @keywords internal
#' @noRd
alpha_grid <- function(num_alpha_values = 10, num_fixed = 6) {
  whole <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x == round(x)
  if (!whole(num_fixed) || num_fixed < 2) {
    stop("`num_fixed` must be a whole number of at least 2.", call. = FALSE)
  }
  if (!whole(num_alpha_values) || num_alpha_values < num_fixed) {
    stop("`num_alpha_values` must be a whole number no smaller than `num_fixed`.", call. = FALSE)
  }
  fixed <- seq(0, 1, length.out = num_fixed)
  n_random <- num_alpha_values - num_fixed
  gap <- (seq_len(n_random) - 1) %% (num_fixed - 1) + 1
  sort(c(fixed, stats::runif(n_random, min = fixed[gap], max = fixed[gap + 1])))
}

#' A yardstick Metric Set From Metric Names
#'
#' @param names Names of exported yardstick survival metrics.
#' @keywords internal
#' @noRd
yardstick_metric_set <- function(names) {
  ns <- asNamespace("yardstick")
  known <- vapply(names, function(n) exists(n, envir = ns, inherits = FALSE) && is.function(get(n, envir = ns)), logical(1))
  if (!is.character(names) || !all(known)) {
    stop("`metric` must name a yardstick survival metric, such as \"brier_survival_integrated\".", call. = FALSE)
  }
  metrics <- do.call(yardstick::metric_set, lapply(names, as.name), envir = ns)
  surv_metric_info(metrics)
  metrics
}

#' Stop Unless Suggested Packages Are Installed
#'
#' @keywords internal
#' @noRd
require_packages <- function(pkgs, fn) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("`", fn, "()` requires package(s): ", paste(missing, collapse = ", "),
         ". Install with install.packages().", call. = FALSE)
  }
  invisible(TRUE)
}
