# Engine-agnostic internals shared by the missforest_* and missranger_*
# mtry sweeps. Both engines sweep the same grid, score per column, and
# assemble each column from whichever run won it; only the fitting step and
# the meaning of "error" differ.


#' Refuse Columns That Are Entirely `NA`
#'
#' Both engines mishandle all-`NA` columns, in different ways:
#'
#' * [missForest::missForest()] silently *drops* them ("removed variable(s) N
#'   due to the missingness of all entries"), so its `OOBerror` vector comes
#'   back shorter than `ncol(data)` and the per-column error table cannot be
#'   built -- surfacing as an opaque tibble recycling error far from the cause.
#' * [missRanger::missRanger()] silently *keeps* them, still entirely `NA`,
#'   returning a frame that looks imputed but is not.
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
    ". Neither engine handles these safely -- `missForest()` drops them and ",
    "`missRanger()` leaves them NA -- and either way the predictor set the ",
    "swept `mtry` values index into would silently shrink. Drop or fill ",
    "these columns before imputing.",
    call. = FALSE
  )
}


#' Pull Each Column From the Run That Won It
#'
#' @param sweep A named list of per-`mtry` results, each with an `ximp`
#'   element, named by `mtry` as a character string.
#' @param best A data frame with one row per column, containing `column` and
#'   `mtry`.
#' @return A tibble of the winning columns, one per row of `best`.
#'
#' @details
#' Runs are looked up by **name** (`sweep[[as.character(mtry)]]`), not by
#' position. Positional lookup happens to work only when the swept `mtry`
#' values are exactly `1:n`; it silently returns the wrong run for any other
#' sweep, such as one starting above 1 or skipping values.
#'
#' @keywords internal
#' @noRd
assemble_by_best_mtry <- function(sweep, best) {
  if (!is.data.frame(best) || !all(c("column", "mtry") %in% names(best))) {
    stop("`best` must be a data frame with `column` and `mtry` columns.", call. = FALSE)
  }
  if (nrow(best) == 0L) {
    return(tibble::tibble())
  }

  pieces <- purrr::map(unique(best$mtry), function(m) {
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


#' Validate and Normalise a Swept `mtry` Grid
#'
#' The admissible upper bound differs by engine, so it is passed in rather
#' than derived from `ncol()`. `missForest` predicts each column from every
#' other one, so its bound is `ncol - 1`; `missRanger` builds its predictor
#' pool up over the first iteration, so its bound is lower -- see
#' [missranger_max_mtry()].
#'
#' @param mtry_values Integer vector, or `NULL` for the default
#'   `1:max_mtry`.
#' @param max_mtry Largest admissible `mtry` for this engine and data.
#' @param note Optional sentence appended to the error, explaining the bound.
#' @return An integer vector named by its own values, so downstream lookups
#'   are by name rather than position.
#' @keywords internal
#' @noRd
normalize_mtry_values <- function(mtry_values, max_mtry, note = NULL) {
  max_mtry <- as.integer(max_mtry)
  if (is.na(max_mtry) || max_mtry < 1L) {
    stop("No admissible `mtry` values for this data.", call. = FALSE)
  }
  if (is.null(mtry_values)) {
    mtry_values <- seq_len(max_mtry)
  }
  mtry_values <- as.integer(mtry_values)
  if (anyNA(mtry_values) || any(mtry_values < 1) || any(mtry_values > max_mtry)) {
    stop(
      "`mtry_values` must be integers in 1:", max_mtry, ".",
      if (!is.null(note)) paste0(" ", note) else "",
      call. = FALSE
    )
  }
  names(mtry_values) <- as.character(mtry_values)
  mtry_values
}


#' Keep the Lowest-Error `mtry` For Each Column
#'
#' @param oob_error A tibble with `column`, `error`, and `mtry`.
#' @return One row per column; ties broken by first occurrence, which is the
#'   lowest `mtry` because the sweep is evaluated in ascending order.
#' @keywords internal
#' @noRd
best_mtry_per_column <- function(oob_error) {
  oob_error |>
    dplyr::group_by(.data$column) |>
    dplyr::filter(.data$error == min(.data$error, na.rm = TRUE)) |>
    dplyr::filter(dplyr::row_number() == 1L) |>
    dplyr::ungroup()
}
