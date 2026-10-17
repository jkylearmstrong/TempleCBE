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

  if (all(train[[status_col]] == 1)) {
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

  fit_and_score <- function() {
    if (length(selected) == 0L) {
      stop("No requested `feature_names` were found in the baked training data.", call. = FALSE)
    }
    y_train <- survival::Surv(train[[start_col]], train[[stop_col]], train[[status_col]])
    y_test <- survival::Surv(test[[start_col]], test[[stop_col]], test[[status_col]])
    # A bootstrap analysis set repeats subjects: score each copy as its own
    # subject, but keep a subject's copies in the same internal fold.
    subject_train <- subject_keys(y_train, train[[id_col]])
    subject_test <- subject_keys(y_test, test[[id_col]])
    truth_train <- surv_subject_truth(y_train, subject_train)
    truth_test <- surv_subject_truth(y_test, subject_test)

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
        graf_weights(truth_test$.truth, effective_eval_time, censoring_km(truth_train$.truth))
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
        graf_weights(truth_test$.truth, cv$eval_time, censoring_km(truth_train$.truth))
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
        shim_env <- list2env(rec_list, parent = orig_env)
        q <- rlang::quo_set_env(q, shim_env)
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
                                           extra = list()) {
  pkg_ver <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.6")
  payload <- list(
    version = pkg_ver,
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
                                     checkpoint_mismatch = c("recompute", "error")) {
  checkpoint_mismatch <- match.arg(checkpoint_mismatch)
  if (is.null(ckpt_file) || !file.exists(ckpt_file)) {
    return(list(status = "missing", value = NULL))
  }
  cached <- tryCatch(readRDS(ckpt_file), error = function(e) NULL)
  if (!is.data.frame(cached) || nrow(cached) == 0L) {
    return(list(status = "invalid", value = NULL))
  }
  found_fp <- attr(cached, "checkpoint_fingerprint", exact = TRUE)
  if (identical(found_fp, expected_fp)) {
    return(list(status = "hit", value = cached))
  }
  msg <- sprintf(
    "Checkpoint '%s' has a mismatched fingerprint (inputs, recipe, features, censoring_weights, or settings changed).",
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
write_verified_checkpoint <- function(obj,
                                      ckpt_file,
                                      fingerprint,
                                      run_fingerprint = fingerprint,
                                      meta = list()) {
  attr(obj, "checkpoint_fingerprint") <- fingerprint
  attr(obj, "checkpoint_run_fingerprint") <- run_fingerprint
  meta$task_fingerprint <- fingerprint
  meta$run_fingerprint <- run_fingerprint
  meta$version <- tryCatch(as.character(utils::packageVersion("TempleCBE")), error = function(e) "0.5.6")
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
  if (!is.numeric(shard) || length(shard) != 2L || any(is.na(shard)) ||
      any(shard != round(shard)) || shard[2] < 1 || shard[1] < 1 || shard[1] > shard[2]) {
    stop("`shard` must be an integer vector `c(i, n)` with `1 <= i <= n`.", call. = FALSE)
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
#' @param checkpoint_mismatch Action when a checkpoint file exists in
#'   `checkpoint_dir` but its stored fingerprint does not match the current
#'   inputs/settings: `"recompute"` (default; warns and recomputes the task) or
#'   `"error"` (stops immediately).
#' @return A list named by `alpha`, one element per fit, each the output of
#'   [purrr::safely()]: a list with `result` (the [glmnet_IBS()] tibble, or
#'   `NULL`) and `error` (`NULL`, or the condition).
#' @seealso [glmnet_IBS()], [summarize_tune_results()], [cv_coxnet()]
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
                            checkpoint_mismatch = c("recompute", "error")) {
  require_packages("furrr", "tune_over_alpha")
  checkpoint_mismatch <- match.arg(checkpoint_mismatch)
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
        warning(sprintf(
          "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed.",
          sum(mismatches), checkpoint_dir
        ), call. = FALSE)
      }
    }

    runs <- furrr::future_pmap(
      list(a = alphas, f = formulas, ckpt_file = ckpt_files, task_fp = task_fps, item_idx = seq_along(alphas)),
      purrr::safely(function(a, f, ckpt_file, task_fp, item_idx) {
        if (!is.null(ckpt_file) && file.exists(ckpt_file)) {
          chk <- read_verified_checkpoint(ckpt_file, task_fp, checkpoint_mismatch = "recompute")
          if (identical(chk$status, "hit")) {
            return(chk$value)
          }
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
      warning(sprintf(
        "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed.",
        sum(mismatches), checkpoint_dir
      ), call. = FALSE)
    }
  }

  runs <- furrr::future_pmap(
    list(a = as.numeric(alphas), ckpt_file = ckpt_files, task_fp = task_fps, item_idx = seq_along(alphas)),
    purrr::safely(function(a, ckpt_file, task_fp, item_idx) {
      if (!is.null(ckpt_file) && file.exists(ckpt_file)) {
        chk <- read_verified_checkpoint(ckpt_file, task_fp, checkpoint_mismatch = "recompute")
        if (identical(chk$status, "hit")) {
          return(chk$value)
        }
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
  names(runs) <- names(alphas)
  runs
}

#' Tune Over `alpha` for Every Split of a Resample
#'
#' Evaluates candidate `(split, alpha)` (or `(split, formula)`) combinations
#' as a single flat task grid across all available [future::plan()] workers and
#' row-binds the successful fits.
#'
#' @section Nested Parallelism & Benchmarking:
#' When tuning penalized survival models across nested resampling structures, computation
#' scales multiplicatively: \eqn{\text{outer folds} \times \text{inner splits} \times \text{alphas} \times \text{internal CV folds}}.
#' `summarize_tune_results()` flattens `inner splits x alphas` into a single task
#' pool (e.g. 7 splits x 10 alphas = 70 tasks) so all cores on high-core machines
#' are fully utilized. Set \code{future::plan(multisession, workers = N)} before
#' calling while keeping inner model evaluations sequential (\code{parallel = FALSE}
#' in \code{glmnet_IBS()}).
#'
#' @param object A resampling object with a `splits` column (e.g. one element
#'   of the `inner_resamples` column of [rsample::nested_cv()]).
#' @inheritParams tune_over_alpha
#' @param seed Optional integer seed for reproducible random draws across splits.
#' @param checkpoint_dir Optional directory path to save checkpoint files.
#'   Finished splits/tasks are loaded from disk on rerun when their fingerprint
#'   matches, skipping recomputation.
#' @param checkpoint_mismatch Action when an existing checkpoint file's
#'   fingerprint does not match the current inputs/settings: `"recompute"`
#'   (default; warns and recomputes) or `"error"` (stops immediately).
#' @param shard Optional integer vector `c(i, n)` specifying that this call
#'   should process shard `i` of `n` across machines into `checkpoint_dir`.
#'   Use [collect_checkpoints()] to merge all shards once finished.
#' @return A tibble of [glmnet_IBS()] results with an `inner_resamples_splits`
#'   column giving the split's identifier.
#' @seealso [glmnet_IBS()], [tune_over_alpha()], [collect_checkpoints()], [nested_cv_coxnet()]
#' @export
summarize_tune_results <- function(object,
                                   ...,
                                   num_alpha_values = 10,
                                   num_fixed = 6,
                                   alphas = NULL,
                                   formulas = NULL,
                                   progress = FALSE,
                                   seed = NULL,
                                   checkpoint_dir = NULL,
                                   checkpoint_mismatch = c("recompute", "error"),
                                   shard = NULL) {
  require_packages("furrr", "summarize_tune_results")
  checkpoint_mismatch <- match.arg(checkpoint_mismatch)
  shard <- validate_shard_spec(shard)
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
    if (length(mismatch_msgs) > 0L) {
      warning(sprintf(
        "%d checkpoint file(s) in '%s' had mismatched fingerprints and will be recomputed. First mismatch: %s",
        length(mismatch_msgs), checkpoint_dir, mismatch_msgs[1]
      ), call. = FALSE)
    }
  }

  furrr_pkgs <- furrr_worker_packages()

  run_one_flat_task <- function(k) {
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

    # 3. Run glmnet_IBS for this (split, alpha/formula) task
    if (!is.na(row$fit_seed)) {
      local_seed(row$fit_seed)
    }
    call_args <- c(list(object = object$splits[[sp_idx]], alpha = a), dots)
    if (!is.null(f)) {
      call_args$formula <- f
    }

    fit_warns <- character()
    fit_err <- NULL
    res <- tryCatch(
      withCallingHandlers(
        do.call(glmnet_IBS, call_args),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("glmnet_IBS\\(\\) could not fit", msg)) {
            fit_warns <<- c(fit_warns, msg)
            invokeRestart("muffleWarning")
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

    list(
      task_idx = k, split_idx = sp_idx, split_id = s_id, item_idx = item_idx,
      ok = is_ok, error_msg = err_reason, result = res
    )
  }

  task_outcomes <- furrr::future_map(
    active_indices,
    run_one_flat_task,
    .options = furrr::furrr_options(seed = TRUE, packages = furrr_pkgs),
    .progress = progress
  )

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
          fn = "summarize_tune_results_split",
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

  res_all <- dplyr::bind_rows(res_by_split)
  if (!is.data.frame(res_all) || nrow(res_all) == 0L || all(is.na(res_all$IBS))) {
    stop("All model fits across all splits failed in summarize_tune_results().", call. = FALSE)
  }

  attr(res_all, "checkpoint_run_fingerprint") <- run_fp
  res_all
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
#' all checkpoint files carry the same `checkpoint_run_fingerprint` so results
#' from different data, recipes, feature sets, or settings cannot be combined
#' by mistake.
#'
#' @param checkpoint_dir Directory containing `.rds` checkpoint files.
#' @param type Optional checkpoint type to collect: `"auto"` (default; inferred
#'   from files in `checkpoint_dir`), `"tune"` ([summarize_tune_results()]), or
#'   `"score_outer"` ([score_outer()]).
#' @param allow_incomplete Logical (default `FALSE`). When `FALSE`, stops with
#'   an error if any tasks from the full run grid are missing.
#' @return A merged tibble matching the return format of
#'   [summarize_tune_results()] or [score_outer()].
#' @seealso [summarize_tune_results()], [score_outer()]
#' @export
collect_checkpoints <- function(checkpoint_dir,
                                type = c("auto", "tune", "score_outer"),
                                allow_incomplete = FALSE) {
  type <- match.arg(type)
  if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L || !dir.exists(checkpoint_dir)) {
    stop("`checkpoint_dir` must be an existing directory path.", call. = FALSE)
  }

  task_tune_files <- sort(list.files(checkpoint_dir, pattern = "^tune_task_split_.*\\.rds$", full.names = TRUE))
  split_tune_files <- sort(list.files(checkpoint_dir, pattern = "^tune_split_.*\\.rds$", full.names = TRUE))
  score_files <- sort(list.files(checkpoint_dir, pattern = "^score_fold_.*\\.rds$", full.names = TRUE))

  if (type == "auto") {
    has_tune <- (length(task_tune_files) + length(split_tune_files)) > 0L
    has_score <- length(score_files) > 0L
    if (has_tune && has_score) {
      stop("`checkpoint_dir` contains both tuning and `score_outer` checkpoints; specify `type = \"tune\"` or `type = \"score_outer\"`.", call. = FALSE)
    }
    if (!has_tune && !has_score) {
      stop(sprintf("No checkpoint files found in '%s'.", checkpoint_dir), call. = FALSE)
    }
    type <- if (has_tune) "tune" else "score_outer"
  }

  files <- if (type == "tune") {
    if (length(task_tune_files) > 0L) task_tune_files else split_tune_files
  } else {
    score_files
  }

  if (length(files) == 0L) {
    stop(sprintf("No '%s' checkpoint files found in '%s'.", type, checkpoint_dir), call. = FALSE)
  }

  objs <- lapply(files, function(f) {
    obj <- tryCatch(readRDS(f), error = function(e) {
      stop(sprintf("Failed to read checkpoint file '%s': %s", basename(f), conditionMessage(e)), call. = FALSE)
    })
    if (!is.data.frame(obj)) {
      stop(sprintf("Checkpoint file '%s' does not contain a data frame.", basename(f)), call. = FALSE)
    }
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

  merged <- dplyr::bind_rows(objs)
  attr(merged, "checkpoint_run_fingerprint") <- unique_fps[1]
  merged
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
    base_dots <- args[setdiff(names(args), c("object", "", "num_alpha_values", "num_fixed", "alphas", "formulas", "progress", "seed", "checkpoint_dir", "checkpoint_mismatch", "shard"))]
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
    base_dots <- args[setdiff(names(args), c("object", "", "num_alpha_values", "num_fixed", "alphas", "formulas", "progress", "seed", "checkpoint_dir", "checkpoint_mismatch"))]
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

  out <- tibble::tibble(
    fn = fn_name,
    n_sample = length(sample_times),
    mean_sec_per_fit = mean_sec,
    median_sec_per_fit = med_sec,
    sd_sec_per_fit = sd_sec,
    n_tasks = as.integer(n_tasks),
    n_total_fits = n_total_fits,
    workers = workers,
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
