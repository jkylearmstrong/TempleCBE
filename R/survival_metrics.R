#' Integrated Brier Score of a Penalized Cox Model on Start/Stop Survival Data
#'
#' Fits a cross-validated penalized Cox model ([glmnet::cv.glmnet()]) on the
#' analysis set of one resampling split and scores it on the assessment set by
#' the integrated Brier score (IBS). Lower is better.
#'
#' @details
#' Data are in counting-process (start/stop) layout: one row per subject per
#' interval, identified by `id_col`, with interval bounds `start_col` and
#' `stop_col` and an event indicator `status_col`.
#'
#' 1. **Preprocessing.** `recipe` is prepped on the analysis set -- only rows
#'    where `start_col == 0` (each subject's first interval) when
#'    `prep_on = "baseline"` -- and baked onto both sets, so normalization is
#'    learned inside the fold. The recipe must keep `id_col`, `start_col`,
#'    `stop_col` and `status_col` in its output (e.g. as `"id variable"` roles).
#' 2. **Fit.** `cv.glmnet(family = "cox")` on the baked `feature_names`
#'    (optionally narrowed by `formula`), choosing `lambda.min` by
#'    `type.measure` over `internal_folds` folds.
#' 3. **Predicted survival.** \eqn{S(t \mid x) = \exp(-H_0(t) \cdot RR)}, where
#'    \eqn{H_0} is the cumulative baseline hazard of a null Cox model fit to
#'    the analysis set, linearly interpolated onto the stop times of
#'    `time_data`, and \eqn{RR} is the relative risk implied by the model's
#'    linear predictor on the assessment set. At grid times where a subject has
#'    no interval, status is carried forward and \eqn{RR} is filled from that
#'    subject's last known value.
#' 4. **Scoring** with [yardstick::brier_survival_integrated()], by
#'    `censoring_weights`:
#'    * `"none"`: every assessment interval row is scored against its own
#'      `Surv(start, stop, status)` with a censoring weight of 1. This
#'      reproduces the analysis code this function was ported from. It does not
#'      adjust for censoring, so subjects censored early count as much as
#'      subjects followed to the end.
#'    * `"ipcw"`: one row per subject, scored against
#'      `Surv(last stop, status at last stop)` and weighted by the inverse
#'      probability of censoring (Graf et al., 1999). Subjects still at risk at
#'      \eqn{t} get weight \eqn{1/G(t)}; subjects with an event at
#'      \eqn{T_i \le t} get \eqn{1/G(T_i^-)}. \eqn{G} is the Kaplan-Meier
#'      censoring distribution of the analysis-set subjects, floored at 0.001.
#'
#' The two settings give different numbers and should not be compared with
#' each other; compare `alpha`/`lambda` within one setting.
#'
#' @param object An `rsplit` (e.g. one element of `rsample` resamples).
#' @param alpha Elastic net mixing parameter: 1 is lasso, 0 is ridge.
#' @param recipe An unprepped [recipes::recipe()].
#' @param feature_names Character vector of baked predictor column names, or a
#'   function that takes the baked analysis set and returns them -- for
#'   recipes whose output columns vary by fold, such as
#'   `step_pca(threshold = )`.
#' @param time_data A data frame of the time grid to evaluate on, with columns
#'   `start_col` and `stop_col` (typically the distinct start/stop pairs of the
#'   full data). A stop time must not appear with more than one start time.
#' @param formula Optional character string of `+`-separated feature names
#'   (or a one-sided formula) to restrict the model to a subset of
#'   `feature_names`. Defaults to all of them.
#' @param internal_folds Number of cross-validation folds inside `cv.glmnet()`.
#' @param id_col,start_col,stop_col,status_col Column names of the subject
#'   identifier, interval start, interval stop, and event indicator (1 = event).
#' @param censoring_weights `"none"` (default) or `"ipcw"`. See Details.
#' @param prep_on `"baseline"` (default) preps `recipe` on analysis rows with
#'   `start_col == 0`; `"all"` preps on every analysis row.
#' @param type.measure Loss used by `cv.glmnet()` to choose lambda (default
#'   `"C"`, Harrell's concordance).
#' @param parallel Passed to `cv.glmnet()`; requires a registered `foreach`
#'   backend.
#' @param failure_ibs IBS reported when `cv.glmnet()` fails to fit (default 2,
#'   outside the valid 0-1 range so failed fits are easy to filter out).
#' @param ... Further arguments passed to [glmnet::cv.glmnet()].
#' @return A tibble with columns `IBS`, `lambda`, `term`, `estimate`, and
#'   `alpha`: one row per coefficient at `lambda.min`. When the fit fails, a
#'   single row with `IBS = failure_ibs`, `lambda = 0`, and `alpha`.
#' @references Graf E, Schmoor C, Sauerbrei W, Schumacher M (1999). Assessment
#'   and comparison of prognostic classification schemes for survival data.
#'   *Statistics in Medicine*, 18(17-18), 2529-2545.
#' @seealso [tune_over_alpha()], [summarize_tune_results()]
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("yardstick", quietly = TRUE) &&
#'     requireNamespace("rsample", quietly = TRUE)) {
#'   set.seed(1)
#'   # Synthetic start/stop data: 40 subjects, up to 3 intervals each
#'   long <- do.call(rbind, lapply(1:40, function(i) {
#'     k <- sample(1:3, 1)
#'     stops <- c(5, 10, 20)[1:k]
#'     data.frame(id = i, tstart = c(0, head(stops, -1)), tstop = stops,
#'                status = c(rep(0, k - 1), rbinom(1, 1, 0.5)),
#'                x1 = rnorm(k), x2 = rnorm(k))
#'   }))
#'   rec <- recipes::recipe(~ ., data = long) |>
#'     recipes::update_role(id, tstart, tstop, status, new_role = "id variable") |>
#'     recipes::step_range(recipes::all_numeric_predictors())
#'   split <- rsample::group_initial_split(long, group = id)
#'   times <- unique(long[, c("tstart", "tstop")])
#'   glmnet_IBS(split, alpha = 0.5, recipe = rec, feature_names = c("x1", "x2"),
#'              time_data = times, internal_folds = 3,
#'              censoring_weights = "ipcw")
#' }
#' }
glmnet_IBS <- function(object,
                       alpha = 1,
                       recipe,
                       feature_names,
                       time_data,
                       formula = NULL,
                       internal_folds = 5,
                       id_col = "id",
                       start_col = "tstart",
                       stop_col = "tstop",
                       status_col = "status",
                       censoring_weights = c("none", "ipcw"),
                       prep_on = c("baseline", "all"),
                       type.measure = "C",
                       parallel = FALSE,
                       failure_ibs = 2,
                       ...) {
  censoring_weights <- match.arg(censoring_weights)
  prep_on <- match.arg(prep_on)
  require_packages(c("glmnet", "survival", "rsample", "yardstick"), "glmnet_IBS")
  if (!inherits(object, "rsplit")) {
    stop("`object` must be an rsplit (e.g. one split of an rsample resample).", call. = FALSE)
  }
  missing_time_cols <- setdiff(c(start_col, stop_col), names(time_data))
  if (length(missing_time_cols) > 0) {
    stop("`time_data` is missing column(s): ", paste(missing_time_cols, collapse = ", "), call. = FALSE)
  }

  train_raw <- rsample::analysis(object)
  test_raw <- rsample::assessment(object)
  prep_data <- if (prep_on == "baseline") {
    dplyr::filter(train_raw, .data[[start_col]] == 0)
  } else {
    train_raw
  }
  prepped <- recipes::prep(recipe, training = prep_data)
  train <- recipes::bake(prepped, new_data = train_raw)

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
  } else if (inherits(formula, "formula")) {
    formula <- paste(deparse(formula[[length(formula)]]), collapse = " ")
  }
  selected <- trimws(strsplit(paste0(formula, collapse = " + "), "\\+")[[1]])
  selected <- intersect(selected, feature_names)

  x_cols <- intersect(colnames(train), selected)
  x_train <- as.matrix(train[, x_cols, drop = FALSE])
  y_train <- survival::Surv(train[[start_col]], train[[stop_col]], train[[status_col]])

  cv_fit <- tryCatch(
    glmnet::cv.glmnet(
      x = x_train, y = y_train, family = "cox", alpha = alpha,
      nfolds = internal_folds, type.measure = type.measure, parallel = parallel, ...
    ),
    error = function(e) NULL
  )
  if (is.null(cv_fit)) {
    return(tibble::tibble(IBS = failure_ibs, lambda = 0, alpha = alpha))
  }

  lambda_min <- cv_fit$lambda.min
  coefs <- broom::tidy(cv_fit$glmnet.fit)
  coefs <- dplyr::filter(coefs, .data$lambda == broom::glance(cv_fit)$lambda.min)
  coefs <- dplyr::select(coefs, "term", "estimate", "lambda")

  test <- recipes::bake(prepped, new_data = test_raw)
  x_test <- as.matrix(test[, colnames(x_train), drop = FALSE])
  test$LinearPredictor <- stats::predict(cv_fit, x_test, type = "link", s = "lambda.min")[, 1]

  # Relative risk: the model's risk against a null model's, both on the
  # assessment set. The offset model re-centres the linear predictor the same
  # way coxph() centres any fitted risk.
  surv_lhs <- sprintf(
    "survival::Surv(%s, %s, %s)",
    backtick(start_col), backtick(stop_col), backtick(status_col)
  )
  id_sym <- rlang::sym(id_col)
  base_cox <- rlang::inject(survival::coxph(
    stats::as.formula(paste(surv_lhs, "~ 1")),
    data = train, id = !!id_sym, model = TRUE
  ))
  offset_cox <- rlang::inject(survival::coxph(
    stats::as.formula(paste(surv_lhs, "~ offset(LinearPredictor)")),
    data = test, id = !!id_sym
  ))
  test$relative_risk <- stats::predict(offset_cox, test, type = "risk") /
    stats::predict(base_cox, test, type = "risk")

  # Baseline cumulative hazard on the evaluation grid, one grid per subject.
  base_fit <- survival::survfit(base_cox, newdata = time_data)
  grid_times <- sort(unique(time_data[[stop_col]]))
  interp <- stats::approx(
    x = base_fit$time, y = base_fit$cumhaz, xout = grid_times,
    method = "linear", rule = 2
  )
  cumhaz <- stats::setNames(tibble::tibble(interp$x, interp$y), c(stop_col, ".cumhaz"))
  grid <- dplyr::left_join(time_data, cumhaz, by = stop_col)
  grid <- purrr::map_dfr(sort(unique(test[[id_col]])), function(id) {
    grid[[id_col]] <- id
    dplyr::relocate(grid, dplyr::all_of(id_col))
  })

  start_right <- paste0(start_col, ".right")
  test <- dplyr::full_join(test, grid, by = c(id_col, stop_col), suffix = c("", ".right"))
  test <- dplyr::arrange(test, .data[[id_col]], .data[[stop_col]])
  test <- dplyr::group_by(test, dplyr::across(dplyr::all_of(id_col)))
  test <- tidyr::fill(test, dplyr::all_of(status_col), .direction = "down")
  test <- dplyr::ungroup(test)
  test[[start_col]] <- dplyr::if_else(is.na(test[[start_col]]), test[[start_right]], test[[start_col]])

  test <- fill_relative_risk(test, id_col)
  test$.eval_time <- test[[stop_col]]
  test$.pred_survival <- exp(-test$.cumhaz * test$relative_risk)

  eval_df <- if (censoring_weights == "none") {
    ibs_rows_unweighted(test, id_col, start_col, stop_col, status_col)
  } else {
    ibs_rows_ipcw(test, train_raw, test_raw, grid_times, id_col, stop_col, status_col)
  }

  IBS <- rlang::inject(yardstick::brier_survival_integrated(
    eval_df,
    truth = !!rlang::sym(".truth"),
    !!rlang::sym(".pred")
  ))$.estimate

  out <- tibble::tibble(IBS = IBS, lambda = lambda_min)
  out <- dplyr::left_join(out, coefs, by = "lambda")
  out$alpha <- alpha
  out
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
#' @param object An `rsplit`.
#' @param ... Arguments passed to [glmnet_IBS()] (`recipe`, `feature_names`,
#'   `time_data`, `id_col`, `censoring_weights`, ...).
#' @param num_alpha_values Total number of `alpha` values in the grid.
#' @param num_fixed Number of evenly spaced values from 0 to 1 in the grid.
#' @param alphas Optional numeric vector of `alpha` values to use instead of
#'   the generated grid; in formula mode, one per formula.
#' @param formulas Optional character vector of `+`-separated feature sets,
#'   each fit as a separate model (see [glmnet_IBS()]'s `formula`).
#' @param progress Show a progress bar.
#' @return A list named by `alpha`, one element per fit, each the output of
#'   [purrr::safely()]: a list with `result` (the [glmnet_IBS()] tibble, or
#'   `NULL`) and `error` (`NULL`, or the condition).
#' @seealso [glmnet_IBS()], [summarize_tune_results()]
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
    formulas <- as.character(formulas)
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
#' @seealso [glmnet_IBS()], [tune_over_alpha()]
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

#' Fill Missing Relative Risks From Each Subject's Last Known Value
#'
#' Repeatedly replaces each missing `relative_risk` with the mean of the
#' subject's last known value and the previous row's value, until none are
#' missing. Rows must be ordered by subject and time.
#'
#' @param data A data frame with a `relative_risk` column.
#' @param id_col Subject identifier column name.
#' @return `data` with `relative_risk` filled.
#' @keywords internal
#' @noRd
fill_relative_risk <- function(data, id_col) {
  n_missing <- sum(is.na(data$relative_risk))
  while (n_missing > 0) {
    known <- dplyr::filter(data, !is.na(.data$relative_risk))
    known <- dplyr::group_by(known, dplyr::across(dplyr::all_of(id_col)))
    known <- dplyr::summarise(known, .last_rr = dplyr::last(.data$relative_risk), .groups = "drop")
    data <- dplyr::left_join(data, known, by = id_col)
    data$relative_risk <- dplyr::if_else(
      is.na(data$relative_risk),
      (data$.last_rr + dplyr::lag(data$relative_risk)) / 2,
      data$relative_risk
    )
    data$.last_rr <- NULL

    remaining <- sum(is.na(data$relative_risk))
    if (remaining >= n_missing) {
      stuck <- unique(data[[id_col]][is.na(data$relative_risk)])
      stop(
        "Could not fill relative risk for subject(s) with no known value: ",
        paste(utils::head(stuck, 10), collapse = ", "),
        if (length(stuck) > 10) ", ...",
        call. = FALSE
      )
    }
    n_missing <- remaining
  }
  data
}

#' Assessment Rows for the Unweighted IBS
#'
#' One row per interval, truth `Surv(start, stop, status)`, each carrying its
#' subject's whole predicted curve with a censoring weight of 1.
#'
#' @keywords internal
#' @noRd
ibs_rows_unweighted <- function(test, id_col, start_col, stop_col, status_col) {
  test$.truth <- survival::Surv(test[[start_col]], test[[stop_col]], test[[status_col]])
  test$.weight_censored <- 1
  pred_cols <- c(".eval_time", ".pred_survival", ".weight_censored")
  preds <- dplyr::select(test, dplyr::all_of(c(id_col, pred_cols)))
  preds <- tidyr::nest(preds, .by = dplyr::all_of(id_col), .key = ".pred")
  rest <- dplyr::select(test, -dplyr::all_of(pred_cols))
  dplyr::left_join(preds, rest, by = id_col)
}

#' Assessment Rows for the IPCW IBS
#'
#' One row per subject, truth `Surv(last stop, status at last stop)`, with a
#' predicted curve on `grid_times` weighted by the inverse probability of
#' censoring estimated from the analysis-set subjects.
#'
#' @keywords internal
#' @noRd
ibs_rows_ipcw <- function(test, train_raw, test_raw, grid_times, id_col, stop_col, status_col) {
  train_subjects <- subject_outcomes(train_raw, id_col, stop_col, status_col)
  test_subjects <- subject_outcomes(test_raw, id_col, stop_col, status_col)
  cens_fit <- survival::survfit(survival::Surv(train_subjects$.time, 1 - train_subjects$.status) ~ 1)

  preds <- dplyr::filter(test, .data$.eval_time %in% grid_times)
  preds <- dplyr::select(preds, dplyr::all_of(id_col), ".eval_time", ".pred_survival")
  preds <- dplyr::distinct(preds, dplyr::across(dplyr::all_of(c(id_col, ".eval_time"))), .keep_all = TRUE)
  preds <- dplyr::inner_join(preds, test_subjects, by = id_col)
  preds <- dplyr::arrange(preds, .data[[id_col]], .data$.eval_time)

  at_risk <- preds$.time > preds$.eval_time
  event_by_t <- !at_risk & preds$.status == 1
  preds$.weight_censored <- 0
  preds$.weight_censored[at_risk] <- 1 / censoring_prob(preds$.eval_time[at_risk], cens_fit)
  preds$.weight_censored[event_by_t] <- 1 / censoring_prob(preds$.time[event_by_t], cens_fit, left = TRUE)

  preds <- dplyr::select(preds, dplyr::all_of(id_col), ".eval_time", ".pred_survival", ".weight_censored")
  preds <- tidyr::nest(preds, .by = dplyr::all_of(id_col), .key = ".pred")
  out <- dplyr::inner_join(test_subjects, preds, by = id_col)
  out$.truth <- survival::Surv(out$.time, out$.status)
  out
}

#' One Row per Subject: Last Observed Stop Time and Status There
#'
#' @keywords internal
#' @noRd
subject_outcomes <- function(data, id_col, stop_col, status_col) {
  data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(id_col)))
  dplyr::summarise(
    data,
    .time = max(.data[[stop_col]]),
    .status = .data[[status_col]][which.max(.data[[stop_col]])],
    .groups = "drop"
  )
}

#' Censoring Survival Probability From a Kaplan-Meier Fit
#'
#' @param t Numeric vector of times.
#' @param cens_fit A [survival::survfit()] of the censoring distribution.
#' @param left If `TRUE`, the left limit \eqn{G(t^-)} (censoring strictly
#'   before `t`); otherwise \eqn{G(t)}.
#' @return Probabilities, floored at 0.001 so weights stay finite.
#' @keywords internal
#' @noRd
censoring_prob <- function(t, cens_fit, left = FALSE) {
  idx <- findInterval(t, cens_fit$time, left.open = left)
  p <- ifelse(idx == 0, 1, cens_fit$surv[pmax(idx, 1)])
  p[is.na(p)] <- 1
  pmax(p, 0.001)
}

#' Quote a Column Name for Use in a Formula String
#'
#' @keywords internal
#' @noRd
backtick <- function(x) paste0("`", gsub("`", "\\\\`", x), "`")

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
