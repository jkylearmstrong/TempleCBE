#' Refuse Columns That Are Entirely `NA`
#'
#' `missForest()` silently *drops* any column with no observed values
#' ("removed variable(s) N due to the missingness of all entries"), so its
#' `OOBerror` vector comes back shorter than `ncol(data)` and the per-column
#' error table cannot be built -- surfacing as an opaque tibble recycling
#' error far from the cause.
#'
#' Refusing is also the right answer on the merits. An all-`NA` column carries
#' no information, yet still counts toward `ncol()` and therefore toward both
#' the `mtry` range and the predictor pool at every split. It passes any
#' name-based schema check, so a batch that silently lost a column would sweep
#' the same nominal `mtry` grid over a strictly weaker predictor set and
#' report success. Dropping it automatically is not available either: the
#' chosen `mtry` values are indices into a predictor set, so quietly shrinking
#' that set changes what they mean.
#'
#' @param data A data frame to check.
#' @param fn Calling function name, used in the error message.
#' @return Invisibly `NULL`; called for its side effect of erroring.
#' @keywords internal
#' @noRd
check_no_all_na_columns <- function(data, fn) {
  all_na <- names(data)[vapply(data, function(x) all(is.na(x)), logical(1))]
  if (length(all_na) == 0L) {
    return(invisible(NULL))
  }
  stop(
    "`", fn, "()` cannot impute: column(s) entirely NA: ",
    paste(all_na, collapse = ", "),
    ". `missForest()` drops all-NA columns, which breaks the per-column error ",
    "table and would silently shrink the predictor set the swept `mtry` ",
    "values index into. Drop or fill these columns before imputing.",
    call. = FALSE
  )
}


#' Run `missForest` at a Single `mtry` and Report Variablewise OOB Error
#'
#' Fits [missForest::missForest()] once at a given `mtry`, returning both the
#' completed data and a tidy, per-column out-of-bag error table. This is the
#' unit of work swept over by [missforest_sweep_mtry()].
#'
#' `missForest` requires every column to be numeric or a factor, so character
#' columns are converted to factors here. The conversion is idempotent -- a
#' caller that has already converted its own characters sees no change --
#' which is what lets this single function replace call sites that previously
#' converted at different points.
#'
#' @param data A data frame or tibble containing the columns to impute. Any
#'   identifier or time columns should already be removed; see
#'   [missforest_sweep_mtry()]'s `exclude` argument. Columns that are entirely
#'   `NA` are refused -- see Details.
#' @param mtry Number of variables randomly sampled as candidates at each
#'   split. A single positive integer.
#' @param ntree Number of trees per forest. Passed to
#'   [missForest::missForest()]. Default `100`.
#' @param maxiter Maximum number of imputation iterations. Passed to
#'   [missForest::missForest()]. Default `10`.
#'
#' @return A list of two elements:
#'   \describe{
#'     \item{`ximp`}{A tibble of the completed (imputed) data.}
#'     \item{`oob_error`}{A tibble with one row per column of `data`, and
#'       columns `column`, `error_type` (`"MSE"` for numeric columns, `"PFC"`
#'       for factors), `error`, and `mtry`.}
#'   }
#'
#' @section Reproducibility:
#' `missForest` is stochastic. This function does not set a seed -- seeding is
#' the caller's responsibility, and [missforest_sweep_mtry()] handles it in a
#' parallel-safe way. See that function's Reproducibility section.
#'
#' @details
#' Columns that are entirely `NA` are refused with an error.
#' [missForest::missForest()] silently drops them, which both breaks the
#' per-column error table this function returns and would quietly shrink the
#' predictor set that swept `mtry` values index into. Such columns arise
#' easily -- a mapping-driven reader that fills absent source columns with a
#' bare `NA` produces full-length logical columns that pass name-based schema
#' checks. Drop or fill them before imputing.
#'
#' @seealso [missforest_sweep_mtry()], [missforest_impute_by_mtry()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("missForest", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(a = c(1, 2, NA, 4, 5), b = c(2, NA, 6, 8, 10))
#'   res <- missforest_oob_by_mtry(df, mtry = 1)
#'   res$oob_error
#' }
#' }
missforest_oob_by_mtry <- function(data, mtry, ntree = 100, maxiter = 10) {
  rlang::check_installed(
    "missForest",
    reason = "to impute missing values with `missforest_oob_by_mtry()`."
  )

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (length(mtry) != 1L || is.na(mtry) || mtry < 1) {
    stop("`mtry` must be a single positive integer.", call. = FALSE)
  }
  check_no_all_na_columns(data, "missforest_oob_by_mtry")

  dt <- as.data.frame(data)

  # missForest rejects character columns; converting here (rather than at the
  # call site) keeps the function total. No-op when already converted.
  is_chr <- vapply(dt, is.character, logical(1))
  if (any(is_chr)) {
    dt[is_chr] <- lapply(dt[is_chr], as.factor)
  }

  ff <- missForest::missForest(
    dt,
    mtry = mtry,
    ntree = ntree,
    maxiter = maxiter,
    variablewise = TRUE
  )

  oob_error <- tibble::tibble(
    column = colnames(dt),
    error_type = names(ff[["OOBerror"]]),
    error = as.numeric(ff[["OOBerror"]]),
    mtry = as.integer(mtry)
  )

  list(
    ximp = tibble::as_tibble(ff[["ximp"]]),
    oob_error = oob_error
  )
}


#' Assemble Imputed Columns From Their Best-`mtry` Runs
#'
#' Given a sweep produced by [missforest_sweep_mtry()] and a table naming the
#' winning `mtry` for each column, pulls each column out of the run that won
#' it. Different columns may come from different runs.
#'
#' @param sweep A named list of [missforest_oob_by_mtry()] results, named by
#'   `mtry` as a character string (as [missforest_sweep_mtry()] builds it).
#' @param best A data frame with one row per column, containing at least the
#'   columns `column` and `mtry` -- typically the `best` element returned by
#'   [missforest_sweep_mtry()].
#'
#' @return A tibble of imputed columns, one per row of `best`.
#'
#' @details
#' Runs are looked up by **name** (`sweep[[as.character(mtry)]]`), not by
#' position. Positional lookup happens to work only when the swept `mtry`
#' values are exactly `1:n`; it silently returns the wrong run for any other
#' sweep, such as one starting above 1 or skipping values.
#'
#' @seealso [missforest_sweep_mtry()]
#' @export
missforest_impute_by_mtry <- function(sweep, best) {
  if (!is.data.frame(best) || !all(c("column", "mtry") %in% names(best))) {
    stop("`best` must be a data frame with `column` and `mtry` columns.", call. = FALSE)
  }

  winning_mtry <- unique(best$mtry)

  pieces <- purrr::map(winning_mtry, function(m) {
    key <- as.character(m)
    run <- sweep[[key]]
    if (is.null(run)) {
      stop(
        "No sweep result named '", key, "'. `sweep` must be named by `mtry`.",
        call. = FALSE
      )
    }
    cols <- best$column[best$mtry == m]
    dplyr::select(tibble::as_tibble(run[["ximp"]]), dplyr::all_of(cols))
  })

  purrr::list_cbind(pieces)
}


#' Impute a Data Frame by Sweeping `missForest` Over `mtry`
#'
#' Runs [missForest::missForest()] once for every candidate `mtry`, then keeps,
#' **for each column independently**, the imputation from whichever `mtry`
#' minimised that column's out-of-bag error. Columns in the same data frame may
#' therefore be imputed at different `mtry` values.
#'
#' @param data A data frame or tibble to impute. After `exclude` is applied,
#'   any remaining column that is entirely `NA` is refused with an error; see
#'   [missforest_oob_by_mtry()]'s Details.
#' @param exclude Character vector of columns to hold out of the imputation
#'   entirely -- typically subject identifiers and time columns. They are
#'   removed before fitting and re-attached, unchanged, to the result. Default
#'   `NULL`.
#' @param mtry_values Integer vector of `mtry` values to sweep. Defaults to
#'   `1:(p - 1)`, where `p` is the number of columns remaining after `exclude`.
#' @param ntree,maxiter Passed to [missforest_oob_by_mtry()].
#' @param seed Controls parallel-safe random number generation; passed to
#'   [furrr::furrr_options()]. `TRUE` (the default) draws reproducible streams
#'   from the current RNG state, so `set.seed()` beforehand makes the whole
#'   sweep reproducible. A single integer seeds the sweep self-containedly,
#'   independent of ambient RNG state. See Reproducibility.
#' @param parallel Whether to evaluate the sweep with [furrr::future_map()].
#'   Default `TRUE`. When `FALSE`, runs sequentially via [purrr::map()].
#'
#' @return A list of three elements:
#'   \describe{
#'     \item{`imp_data`}{A tibble of the imputed data, with `exclude` columns
#'       re-attached and the original column order restored.}
#'     \item{`oob_error`}{A tibble of every column's OOB error at every swept
#'       `mtry`.}
#'     \item{`best`}{The winning row per column -- the subset of `oob_error`
#'       minimising `error` within each `column`, ties broken by first
#'       occurrence.}
#'   }
#'
#' @section Reproducibility:
#' `missForest` is stochastic, so an unseeded sweep is not reproducible. In
#' parallel mode the seed **must** reach [furrr::future_map()]'s `.options`;
#' attaching `furrr_options()` to [future::plan()] instead is silently ignored
#' (`future` warns about an unknown argument and proceeds unseeded). This
#' function always passes `seed` through to the `future_map()` call.
#'
#' Seeded this way, results are independent of the number of workers and of
#' the `future` plan, so a Windows `multisession` run and a Linux `multicore`
#' run draw identical random streams. Note that identical streams do not by
#' themselves guarantee bit-identical output across *architectures*: tree
#' splits compare floating-point impurity sums, and differences in FMA
#' contraction or BLAS between x86_64 and arm64 can resolve a near-tie
#' differently. Verify cross-architecture agreement rather than assuming it.
#'
#' The sequential path (`parallel = FALSE`) seeds with [set.seed()] when
#' `seed` is a number. It is internally reproducible but will not match the
#' parallel path, which uses L'Ecuyer-CMRG streams.
#'
#' @section Parallel plan:
#' This function never calls [future::plan()] -- the appropriate backend
#' differs by machine, and choosing one here would override the caller's. Set
#' a plan before calling, for example
#' `future::plan(future::multisession, workers = future::availableCores() - 1)`.
#'
#' @seealso [missforest_oob_by_mtry()], [missforest_impute_by_mtry()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("missForest", quietly = TRUE)) {
#'   df <- data.frame(
#'     id = 1:8,
#'     a = c(1, 2, NA, 4, 5, 6, NA, 8),
#'     b = c(2, NA, 6, 8, 10, 12, 14, NA),
#'     c = c(5, 4, 3, NA, 1, 2, 3, 4)
#'   )
#'   res <- missforest_sweep_mtry(df, exclude = "id", seed = 42, parallel = FALSE)
#'   res$best
#' }
#' }
missforest_sweep_mtry <- function(data,
                                  exclude = NULL,
                                  mtry_values = NULL,
                                  ntree = 100,
                                  maxiter = 10,
                                  seed = TRUE,
                                  parallel = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  data <- tibble::as_tibble(data)
  original_order <- names(data)

  if (is.null(exclude)) exclude <- character()
  exclude <- intersect(exclude, original_order)
  dt <- dplyr::select(data, -dplyr::all_of(exclude))

  if (ncol(dt) < 2L) {
    stop(
      "Need at least 2 columns to impute after `exclude`; got ", ncol(dt), ".",
      call. = FALSE
    )
  }
  # Checked here as well as in the worker, so a doomed sweep fails before
  # dispatching every `mtry` to the parallel backend.
  check_no_all_na_columns(dt, "missforest_sweep_mtry")

  if (is.null(mtry_values)) {
    mtry_values <- seq_len(ncol(dt) - 1L)
  }
  mtry_values <- as.integer(mtry_values)
  if (anyNA(mtry_values) || any(mtry_values < 1) || any(mtry_values >= ncol(dt))) {
    stop(
      "`mtry_values` must be integers in 1:(ncol - 1) = 1:", ncol(dt) - 1L, ".",
      call. = FALSE
    )
  }
  # Named by mtry so results are looked up by name, never by position.
  names(mtry_values) <- as.character(mtry_values)

  fit_one <- function(m) {
    missforest_oob_by_mtry(dt, mtry = m, ntree = ntree, maxiter = maxiter)
  }

  if (parallel) {
    rlang::check_installed(
      c("furrr", "future"),
      reason = "to run the `mtry` sweep in parallel (or set `parallel = FALSE`)."
    )
    sweep <- furrr::future_map(
      mtry_values,
      fit_one,
      .options = furrr::furrr_options(seed = seed)
    )
  } else {
    if (is.numeric(seed)) {
      set.seed(seed)
    }
    sweep <- purrr::map(mtry_values, fit_one)
  }

  oob_error <- purrr::list_rbind(purrr::map(sweep, "oob_error"))

  best <- oob_error |>
    dplyr::group_by(.data$column) |>
    dplyr::filter(.data$error == min(.data$error, na.rm = TRUE)) |>
    dplyr::filter(dplyr::row_number() == 1L) |>
    dplyr::ungroup()

  imputed <- missforest_impute_by_mtry(sweep, best)

  imp_data <- dplyr::bind_cols(
    dplyr::select(data, dplyr::all_of(exclude)),
    imputed
  )
  # Restore the caller's column order; assembling by winning-mtry group would
  # otherwise leave columns ordered by which run produced them.
  imp_data <- dplyr::select(imp_data, dplyr::all_of(original_order))

  list(
    imp_data = imp_data,
    oob_error = oob_error,
    best = best
  )
}
