#' Add Missing Values Completely at Random
#'
#' Sets a fixed share of cells in each selected column to \code{NA}, choosing
#' the rows independently for each column (missing completely at random). Use
#' it to test imputation: start from complete data, add missingness, impute,
#' and compare the imputed cells with the originals.
#'
#' Every selected column must start with no missing values, so afterwards its
#' share of \code{NA}s equals \code{pct_na} (to the nearest whole row), matching
#' \code{PctNa} from \code{\link{features_percent_miss}}. The observed share is
#' \code{1 - pct_na}, so only \code{pct_na} is given.
#'
#' @param data A data frame or tibble.
#' @param cols Columns to add missing values to, as a tidyselect expression
#'   (default \code{dplyr::everything()}).
#' @param pct_na Proportion of cells to set to \code{NA} in each selected
#'   column, between 0 and 1 (default 0.1). Each column gets
#'   \code{round(pct_na * nrow(data))} missing cells.
#' @return \code{data} with missing values added. Its \code{"missing_cells"}
#'   attribute is a tibble with one row per masked cell, giving its \code{row}
#'   number and \code{feature} (column name). Most dplyr verbs drop this
#'   attribute, so read it before transforming the result.
#' @export
#' @examples
#' set.seed(1)
#' amputed <- add_missing(mtcars, c(mpg, hp), pct_na = 0.25)
#' features_percent_miss(amputed)
#' head(attr(amputed, "missing_cells"))
add_missing <- function(data, cols = dplyr::everything(), pct_na = 0.1) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.numeric(pct_na) || length(pct_na) != 1 || is.na(pct_na) || pct_na < 0 || pct_na > 1) {
    stop("`pct_na` must be a single number between 0 and 1.")
  }

  selected <- names(dplyr::select(data, {{ cols }}))
  already_missing <- selected[vapply(data[selected], anyNA, logical(1))]
  if (length(already_missing) > 0) {
    stop("Selected columns must have no missing values; found NA in: ",
         paste(already_missing, collapse = ", "))
  }

  n_na <- round(pct_na * nrow(data))
  missing_rows <- list()
  for (col in selected) {
    rows <- sort(sample.int(nrow(data), n_na))
    data[[col]][rows] <- NA
    missing_rows[[col]] <- rows
  }

  attr(data, "missing_cells") <- tibble::tibble(
    row = as.integer(unlist(missing_rows, use.names = FALSE)),
    feature = rep(selected, each = n_na)
  )
  data
}
