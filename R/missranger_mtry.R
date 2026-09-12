#' Run `missRanger` at a Single `mtry` and Report Per-Column OOB Error
#'
#' Fits [missRanger::missRanger()] once at a given `mtry`, returning both the
#' completed data and a tidy, per-column out-of-bag error table. This is the
#' unit of work swept over by [missranger_sweep_mtry()], and the `ranger`-based
#' counterpart to [missforest_oob_by_mtry()].
#'
#' @param data A data frame or tibble containing the columns to impute. Any
#'   identifier or time columns should already be removed; see
#'   [missranger_sweep_mtry()]'s `exclude` argument. Columns that are entirely
#'   `NA` are refused -- see Details.
#' @param mtry Number of variables randomly sampled as candidates at each
#'   split. A single positive integer.
#' @param num.trees Number of trees per forest. Passed to
#'   [missRanger::missRanger()]. Default `500`, that function's own default.
#' @param pmm.k Number of predictive-mean-matching donors. `0` (the default)
#'   disables PMM and imputes with the forest prediction directly.
#' @param maxiter Maximum number of imputation iterations. Default `10`.
#' @param seed Integer seed passed to [missRanger::missRanger()], or `NULL`
#'   (default) to leave it unseeded. See Reproducibility.
#' @param num.threads Threads used by `ranger` *within* one fit. Defaults to
#'   `1`; see the Threading section, which explains why the `ranger` default is
#'   the wrong one here.
#' @param keep_forests Whether to retain the fitted forests on the returned
#'   object. Default `FALSE`. See Details.
#'
#' @return A list of two elements, plus a third when `keep_forests = TRUE`:
#'   \describe{
#'     \item{`ximp`}{A tibble of the completed (imputed) data.}
#'     \item{`oob_error`}{A tibble with columns `column`, `error_type`,
#'       `error`, and `mtry` -- one row per *imputed* column. See Details.}
#'     \item{`fit`}{The `missRanger` object, when `keep_forests = TRUE`.}
#'   }
#'
#' @section Threading:
#' `ranger` defaults to using every available core. Inside a parallel `mtry`
#' sweep that nests one thread pool inside another and oversubscribes the
#' machine badly -- workers times cores threads competing for the same cores.
#' `num.threads` therefore defaults to `1` here, leaving parallelism to the
#' sweep. Raise it only when running a single fit on an otherwise idle box.
#'
#' @section Reproducibility:
#' Unlike `missForest`, `missRanger` takes its own `seed` and uses it to seed
#' the fit directly, so a seeded fit is fully determined by `seed` regardless
#' of ambient RNG state, worker count, or `future` plan. That makes seeded
#' `missranger_*` sweeps reproducible by construction rather than by careful
#' stream management. The architecture caveat still applies: identical seeds do
#' not guarantee bit-identical results across x86_64 and arm64, because split
#' selection compares floating-point impurity sums.
#'
#' @details
#' **Error rows cover imputed columns only.** `missForest` run variablewise
#' reports an OOB error for every column; `missRanger` fits forests only for
#' columns that actually have missing values, so complete columns get no row.
#' [missranger_sweep_mtry()] carries those columns through unchanged.
#'
#' **Errors are not comparable across engines.** `missForest` reports raw MSE
#' and PFC. `missRanger` reports `ranger`'s scaled OOB prediction error --
#' `1 - R^2` for numeric targets and classification error for categorical ones
#' -- where roughly `1` means no better than predicting the mean. Compare
#' `mtry` values within one engine; do not compare an `error` column from one
#' against the other.
#'
#' **All-`NA` columns are refused.** `missRanger` does not drop them the way
#' `missForest` does; it silently returns them still entirely `NA`, producing a
#' frame that looks imputed but is not. Such columns arise easily -- a
#' mapping-driven reader that fills absent source columns with a bare `NA`
#' yields full-length logical columns that pass name-based schema checks.
#'
#' **`keep_forests` is the seam for applying to new data.** `missForest`
#' discards its forests, which is why it cannot be applied to a later batch.
#' `missRanger` can retain them, making `predict()` on new data possible. This
#' function only exposes the option; a fit/apply pair built on it is separate
#' work.
#'
#' @seealso [missranger_sweep_mtry()], [missforest_oob_by_mtry()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("missRanger", quietly = TRUE)) {
#'   df <- data.frame(
#'     a = c(1, 2, NA, 4, 5, 6, 7, 8),
#'     b = c(2, NA, 6, 8, 10, 12, 14, 16),
#'     c = c(5, 4, 3, NA, 1, 2, 3, 4)
#'   )
#'   res <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 50, seed = 1)
#'   res$oob_error
#' }
#' }
missranger_oob_by_mtry <- function(data,
                                   mtry,
                                   num.trees = 500,
                                   pmm.k = 0,
                                   maxiter = 10,
                                   seed = NULL,
                                   num.threads = 1,
                                   keep_forests = FALSE) {
  rlang::check_installed(
    "missRanger",
    reason = "to impute missing values with `missranger_oob_by_mtry()`."
  )

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (length(mtry) != 1L || is.na(mtry) || mtry < 1) {
    stop("`mtry` must be a single positive integer.", call. = FALSE)
  }
  check_no_all_na_columns(data, "missranger_oob_by_mtry")

  dt <- as.data.frame(data)

  fit <- missRanger::missRanger(
    dt,
    mtry = mtry,
    num.trees = num.trees,
    pmm.k = pmm.k,
    maxiter = maxiter,
    seed = seed,
    num.threads = num.threads,
    verbose = 0,
    data_only = FALSE,
    keep_forests = keep_forests
  )

  oob_error <- missranger_oob_table(fit, dt, mtry)

  out <- list(
    ximp = tibble::as_tibble(fit[["data"]]),
    oob_error = oob_error
  )
  if (keep_forests) {
    out[["fit"]] <- fit
  }
  out
}


#' Tidy a `missRanger` Fit's Prediction Errors
#'
#' `pred_errors` is an iteration-by-variable matrix whose columns are the
#' imputed variables. The row for `best_iter` is the one `missRanger` selected,
#' and is what `returnOOB = TRUE` would have attached.
#'
#' @param fit A `missRanger` object (from `data_only = FALSE`).
#' @param dt The data frame that was fitted, used to classify error types.
#' @param mtry The `mtry` this fit used.
#' @return A tibble with `column`, `error_type`, `error`, `mtry`.
#' @keywords internal
#' @noRd
missranger_oob_table <- function(fit, dt, mtry) {
  pe <- fit[["pred_errors"]]
  imputed <- colnames(pe)

  # No column had missing values: nothing was fitted, so there is nothing to
  # score. An empty table is correct, not an error.
  if (is.null(pe) || length(imputed) == 0L) {
    return(tibble::tibble(
      column = character(),
      error_type = character(),
      error = numeric(),
      mtry = integer()
    ))
  }

  best_iter <- fit[["best_iter"]]
  if (is.null(best_iter) || best_iter < 1L || best_iter > nrow(pe)) {
    best_iter <- nrow(pe)
  }

  tibble::tibble(
    column = imputed,
    error_type = vapply(
      imputed,
      function(nm) if (is.numeric(dt[[nm]])) "1-R2" else "class_error",
      character(1),
      USE.NAMES = FALSE
    ),
    error = as.numeric(pe[best_iter, ]),
    mtry = as.integer(mtry)
  )
}


#' Largest `mtry` `missRanger` Will Accept For a Data Set
#'
#' `missRanger` admits a **smaller** `mtry` than `missForest` on the same
#' data, and the difference is easy to trip over: exceeding it surfaces as
#' `ranger`'s catch-all `"User interrupt or internal error."`, which names
#' neither `mtry` nor the real bound.
#'
#' @param data A data frame or tibble, after any identifier columns have been
#'   excluded.
#' @return A single integer: the largest admissible `mtry`, at least `1`.
#'
#' @details
#' `missForest` predicts every column from every other one, so its bound is
#' simply `ncol(data) - 1`. `missRanger` builds its predictor pool up over the
#' first iteration instead. It starts with `completed`, the set of
#' **fully-observed** columns, imputes the targets in increasing order of
#' missingness, and adds each one to `completed` as it goes. The first target
#' therefore sees only the complete columns, and `mtry` must fit *that* pool:
#'
#' * With `k` usable complete columns, the bound is `k`.
#' * With none, the first target falls back to univariate imputation and the
#'   second sees a single predictor, so the bound is `1` -- no matter how wide
#'   the data is.
#'
#' Constant columns are excluded from the count, matching `missRanger`'s own
#' handling of them as features.
#'
#' The practical consequence: a sweep is only informative when some columns
#' are complete. That is the usual shape of the intended use -- imputing a
#' domain's incomplete outputs against its fully-observed inputs -- but a
#' frame in which *every* column has missing values admits `mtry = 1` alone,
#' and there is nothing to sweep.
#'
#' @seealso [missranger_sweep_mtry()]
#' @export
#' @examples
#' # Two complete columns -> mtry may be 1 or 2.
#' missranger_max_mtry(data.frame(a = c(1, NA, 3), b = c(1, 2, 3), c = c(4, 5, 6)))
#'
#' # Every column has a gap -> only mtry = 1 is admissible.
#' missranger_max_mtry(data.frame(a = c(1, NA, 3), b = c(NA, 2, 3), c = c(4, 5, NA)))
missranger_max_mtry <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  # Constant features are dropped by missRanger, so they cannot be predictors.
  non_constant <- vapply(
    data, function(z) length(unique(z[!is.na(z)])) > 1L, logical(1)
  )
  has_na <- vapply(data, anyNA, logical(1))
  max(1L, sum(non_constant & !has_na))
}


#' Impute a Data Frame by Sweeping `missRanger` Over `mtry`
#'
#' Runs [missRanger::missRanger()] once for every candidate `mtry`, then keeps,
#' **for each column independently**, the imputation from whichever `mtry`
#' minimised that column's out-of-bag prediction error. The `ranger`-based
#' counterpart to [missforest_sweep_mtry()], with the same return shape so the
#' two engines can be compared on one data set.
#'
#' @param data A data frame or tibble to impute. After `exclude` is applied,
#'   any remaining column that is entirely `NA` is refused with an error.
#' @param exclude Character vector of columns to hold out of the imputation
#'   entirely -- typically subject identifiers and time columns. They are
#'   removed before fitting and re-attached, unchanged, to the result. Default
#'   `NULL`.
#' @param mtry_values Integer vector of `mtry` values to sweep. Defaults to
#'   `1:missranger_max_mtry(data)`. Note this is **not** `1:(p - 1)`, the
#'   `missForest` bound -- see [missranger_max_mtry()] for why, and expect a
#'   shorter sweep than [missforest_sweep_mtry()] performs on the same data.
#' @param num.trees,pmm.k,maxiter,num.threads Passed to
#'   [missranger_oob_by_mtry()].
#' @param seed Integer seed passed to every fit in the sweep, or `NULL`
#'   (default) for an unseeded sweep. See Reproducibility.
#' @param max_pct_missing Optional proportion in `(0, 1]`. Columns missing a
#'   greater share than this are held out of the imputation and carried
#'   through unimputed, and are reported in `excluded_high_missing`. `NULL`
#'   (the default) imputes every column regardless of how sparse it is.
#'   Imputing a column observed in a handful of rows manufactures values
#'   rather than recovering them, and nothing downstream can tell the
#'   difference; a threshold states that judgement as a rule that carries to
#'   the next data set, instead of naming the offending column inline.
#'   Note a threshold below `1` subsumes the all-`NA` refusal, since an
#'   all-`NA` column exceeds every threshold.
#' @param parallel Whether to evaluate the sweep with [furrr::future_map()].
#'   Default `TRUE`. When `FALSE`, runs sequentially via [purrr::map()].
#'
#' @return A list of four elements, matching [missforest_sweep_mtry()]:
#'   \describe{
#'     \item{`imp_data`}{A tibble of the imputed data, with `exclude` columns
#'       re-attached and the original column order restored.}
#'     \item{`oob_error`}{A tibble of every imputed column's OOB error at every
#'       swept `mtry`.}
#'     \item{`best`}{The winning row per imputed column.}
#'     \item{`excluded_high_missing`}{Columns held out by `max_pct_missing`,
#'       carried through unimputed. `character(0)` when none were.}
#'   }
#'
#' @section Reproducibility:
#' The same `seed` is passed to every fit in the sweep. That is deliberate:
#' holding the seed fixed across `mtry` values is a common-random-numbers
#' comparison, so differences in OOB error reflect `mtry` rather than sampling
#' noise between runs. Because `missRanger` seeds itself, a seeded sweep is
#' reproducible independently of the `future` plan and the number of workers --
#' the property a mixed Windows/Linux/arm64 fleet needs -- without depending on
#' L'Ecuyer stream management. `furrr`'s own parallel-safe seeding is still
#' enabled, which covers the unseeded case.
#'
#' @section Parallel plan:
#' This function never calls [future::plan()] -- the appropriate backend
#' differs by machine, and choosing one here would override the caller's. Set a
#' plan before calling. Note that `num.threads` defaults to `1` so that
#' `ranger`'s own threading does not nest inside the sweep; see
#' [missranger_oob_by_mtry()]'s Threading section.
#'
#' @seealso [missforest_sweep_mtry()] for the `missForest` engine.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("missRanger", quietly = TRUE)) {
#'   df <- data.frame(
#'     id = 1:12,
#'     a = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, NA),
#'     b = c(2, 4, 6, NA, 10, 12, 14, NA, 18, 20, NA, 24),
#'     c = c(5, 4, 3, NA, 1, 2, 3, 4, 5, NA, 3, 2)
#'   )
#'   res <- missranger_sweep_mtry(
#'     df, exclude = "id", num.trees = 50, seed = 42, parallel = FALSE
#'   )
#'   res$best
#' }
#' }
missranger_sweep_mtry <- function(data,
                                  exclude = NULL,
                                  mtry_values = NULL,
                                  num.trees = 500,
                                  pmm.k = 0,
                                  maxiter = 10,
                                  seed = NULL,
                                  num.threads = 1,
                                  max_pct_missing = NULL,
                                  parallel = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  data <- tibble::as_tibble(data)
  original_order <- names(data)

  if (is.null(exclude)) exclude <- character()
  exclude <- intersect(exclude, original_order)
  dt <- dplyr::select(data, -dplyr::all_of(exclude))

  # Columns too sparse to impute are held out and carried through unimputed,
  # exactly as `exclude` would -- but derived from the data, so the decision
  # travels to a data set whose sparse columns have different names.
  excluded_high_missing <- high_missing_columns(dt, max_pct_missing)
  if (length(excluded_high_missing) > 0L) {
    message(
      "Holding out ", length(excluded_high_missing),
      " column(s) above max_pct_missing = ", max_pct_missing, ": ",
      paste(excluded_high_missing, collapse = ", "),
      ". They are carried through unimputed."
    )
    exclude <- c(exclude, excluded_high_missing)
    dt <- dplyr::select(dt, -dplyr::all_of(excluded_high_missing))
  }

  if (ncol(dt) < 2L) {
    stop(
      "Need at least 2 columns to impute after `exclude`; got ", ncol(dt), ".",
      call. = FALSE
    )
  }
  # Checked here as well as in the worker, so a doomed sweep fails before
  # dispatching every `mtry` to the parallel backend.
  check_no_all_na_columns(dt, "missranger_sweep_mtry")

  # NOT ncol - 1: missRanger's first-iteration predictor pool is smaller.
  mtry_values <- normalize_mtry_values(
    mtry_values,
    missranger_max_mtry(dt),
    note = paste(
      "missRanger builds its predictor pool up over the first iteration, so",
      "the bound is the number of complete columns, not ncol - 1. See",
      "`?missranger_max_mtry`."
    )
  )

  fit_one <- function(m) {
    missranger_oob_by_mtry(
      dt,
      mtry = m,
      num.trees = num.trees,
      pmm.k = pmm.k,
      maxiter = maxiter,
      seed = seed,
      num.threads = num.threads
    )
  }

  if (parallel) {
    rlang::check_installed(
      c("furrr", "future"),
      reason = "to run the `mtry` sweep in parallel (or set `parallel = FALSE`)."
    )
    sweep <- furrr::future_map(
      mtry_values,
      fit_one,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    sweep <- purrr::map(mtry_values, fit_one)
  }

  oob_error <- purrr::list_rbind(purrr::map(sweep, "oob_error"))
  best <- best_mtry_per_column(oob_error)

  imputed <- assemble_by_best_mtry(sweep, best)

  # Columns with no missing values are never fitted and so never scored; they
  # are identical in every run and carry through from the input untouched.
  carried <- setdiff(names(dt), best$column)

  imp_data <- dplyr::bind_cols(
    dplyr::select(data, dplyr::all_of(exclude)),
    imputed,
    dplyr::select(data, dplyr::all_of(carried))
  )
  # Restore the caller's column order; assembling by winning-mtry group would
  # otherwise leave columns ordered by which run produced them.
  imp_data <- dplyr::select(imp_data, dplyr::all_of(original_order))

  list(
    imp_data = imp_data,
    oob_error = oob_error,
    best = best,
    excluded_high_missing = excluded_high_missing
  )
}
