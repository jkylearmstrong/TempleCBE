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
#' @param censoring_weights Only `"ipcw"` is supported. `"none"`, which scored
#'   interval rows without adjusting for censoring, was removed in TempleCBE
#'   0.3.0; install TempleCBE 0.2.0 to reproduce results that used it.
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
                       prep_on = c("baseline", "all"),
                       eval_time = NULL,
                       metric = "brier_survival_integrated",
                       rule = c("min", "1se"),
                       covariates = c("path", "baseline"),
                       failure_ibs = NA_real_,
                       ...) {
  censoring_weights <- match.arg(censoring_weights)
  if (censoring_weights == "none") {
    stop(
      "`censoring_weights = \"none\"` was removed in TempleCBE 0.3.0: it scored interval rows ",
      "without adjusting for censoring, so it was not a proper Brier score. Use \"ipcw\" (the ",
      "default), or install TempleCBE 0.2.0 to reproduce results that used \"none\".",
      call. = FALSE
    )
  }
  prep_on <- match.arg(prep_on)
  rule <- match.arg(rule)
  covariates <- match.arg(covariates)
  dots <- list(...)
  if ("parallel" %in% names(dots)) {
    dots$parallel <- NULL
    rlang::warn(
      "`parallel` is no longer used by glmnet_IBS(); set a future::plan() for tune_over_alpha() instead.",
      .frequency = "once", .frequency_id = "TempleCBE_glmnet_IBS_parallel"
    )
  }
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
  prepped <- recipes::prep(recipe, training = prep_data)
  train <- recipes::bake(prepped, new_data = train_raw)
  test <- recipes::bake(prepped, new_data = test_raw)

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
    y_train <- survival::Surv(train[[start_col]], train[[stop_col]], train[[status_col]])
    y_test <- survival::Surv(test[[start_col]], test[[stop_col]], test[[status_col]])
    # A bootstrap analysis set repeats subjects: score each copy as its own
    # subject, but keep a subject's copies in the same internal fold.
    subject_train <- subject_keys(y_train, train[[id_col]])
    subject_test <- subject_keys(y_test, test[[id_col]])
    cv <- do.call(cv_coxnet_impl, c(
      list(
        spec_xy(as.data.frame(train[selected]), y_train, subject_train, train[[id_col]]),
        mixture = alpha, v = internal_folds, metrics = select_metrics,
        eval_time = eval_time, metric = metric, covariates = covariates
      ),
      dots
    ))
    chosen <- if (rule == "min") cv$lambda_min else cv$lambda_1se

    truth_train <- surv_subject_truth(y_train, subject_train)
    truth_test <- surv_subject_truth(y_test, subject_test)
    weights <- graf_weights(truth_test$.truth, cv$eval_time, censoring_km(truth_train$.truth))
    scored <- score_coxnet_path(
      cv$fit$fit, cv$fit$x, y_train, predictors_matrix(test[selected]), y_test, subject_test,
      penalty = chosen, eval_time = cv$eval_time, truth = truth_test, weights = weights,
      metrics = ibs_metrics, info = surv_metric_info(ibs_metrics), covariates = covariates
    )
    coefs <- tidy.coxnet_model(cv$fit, penalty = chosen)
    tibble::tibble(IBS = scored$.estimate[1], lambda = chosen, term = coefs$term, estimate = coefs$estimate, alpha = alpha)
  }

  tryCatch(fit_and_score(), error = function(e) {
    warning("glmnet_IBS() could not fit alpha = ", alpha, " and returned `failure_ibs`: ", conditionMessage(e), call. = FALSE)
    tibble::tibble(IBS = failure_ibs, lambda = NA_real_, alpha = alpha)
  })
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
                            progress = FALSE) {
  require_packages("furrr", "tune_over_alpha")
  dots <- list(...)

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
    runs <- furrr::future_map2(
      alphas,
      formulas,
      purrr::safely(function(a, f) {
        res <- do.call(glmnet_IBS, c(list(object = object, alpha = a, formula = f), dots))
        res$formula <- f
        res
      }),
      .options = furrr::furrr_options(seed = TRUE),
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

  furrr::future_map(
    alphas,
    purrr::safely(function(a) {
      do.call(glmnet_IBS, c(list(object = object, alpha = a), dots))
    }),
    .options = furrr::furrr_options(seed = TRUE),
    .progress = progress
  )
}

#' Tune Over `alpha` for Every Split of a Resample
#'
#' Calls [tune_over_alpha()] on each split of `object` and row-binds the
#' successful fits. Fits that errored are dropped. A split draws its own random
#' `alpha` values, reproducibly: the map runs with `furrr_options(seed = TRUE)`.
#'
#' @param object A resampling object with a `splits` column (e.g. one element
#'   of the `inner_resamples` column of [rsample::nested_cv()]).
#' @inheritParams tune_over_alpha
#' @return A tibble of [glmnet_IBS()] results with an `inner_resamples_splits`
#'   column giving the split's position in `object$splits`.
#' @seealso [glmnet_IBS()], [tune_over_alpha()], [nested_cv_coxnet()]
#' @export
summarize_tune_results <- function(object,
                                   ...,
                                   num_alpha_values = 10,
                                   num_fixed = 6,
                                   alphas = NULL,
                                   formulas = NULL,
                                   progress = FALSE) {
  require_packages("furrr", "summarize_tune_results")
  dots <- list(...)

  furrr::future_map_dfr(
    object$splits,
    function(split) {
      runs <- do.call(tune_over_alpha, c(
        list(object = split, num_alpha_values = num_alpha_values,
             num_fixed = num_fixed, alphas = alphas, formulas = formulas),
        dots
      ))
      ok <- vapply(runs, function(run) is.null(run$error), logical(1))
      dplyr::bind_rows(lapply(runs[ok], `[[`, "result"))
    },
    .id = "inner_resamples_splits",
    .options = furrr::furrr_options(seed = TRUE),
    .progress = progress
  )
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
