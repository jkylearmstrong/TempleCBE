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
#'   column, between 0 and 1 (default 0.1). Can be a single number applied to
#'   all selected columns, or a numeric vector with length equal to the number
#'   of selected columns specifying column-specific amputation rates.
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
#'
#' # Column-specific missingness proportions with a vector:
#' p_vec <- c(mpg = 0.1, hp = 0.3)
#' amputed2 <- add_missing(mtcars, c(mpg, hp), pct_na = p_vec)
#' features_percent_miss(amputed2)
add_missing <- function(data, cols = dplyr::everything(), pct_na = 0.1) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }

  selected <- names(dplyr::select(data, {{ cols }}))
  already_missing <- selected[vapply(data[selected], anyNA, logical(1))]
  if (length(already_missing) > 0) {
    stop("Selected columns must have no missing values; found NA in: ",
         paste(already_missing, collapse = ", "))
  }

  if (!is.numeric(pct_na) || anyNA(pct_na) || any(pct_na < 0 | pct_na > 1)) {
    stop("`pct_na` must contain numbers between 0 and 1.")
  }

  if (length(pct_na) == 1) {
    pct_na_vec <- rep(pct_na, length(selected))
  } else if (length(pct_na) == length(selected)) {
    if (!is.null(names(pct_na)) && all(selected %in% names(pct_na))) {
      pct_na_vec <- pct_na[selected]
    } else {
      pct_na_vec <- pct_na
    }
  } else {
    stop(sprintf(
      "`pct_na` must be a single number or a vector of length matching selected columns (%d), got length %d.",
      length(selected), length(pct_na)
    ))
  }

  missing_rows <- vector("list", length(selected))
  names(missing_rows) <- selected

  for (i in seq_along(selected)) {
    col <- selected[i]
    n_na <- round(pct_na_vec[i] * nrow(data))
    rows <- if (n_na > 0) sort(sample.int(nrow(data), n_na)) else integer(0)
    data[[col]][rows] <- NA
    missing_rows[[col]] <- rows
  }

  attr(data, "missing_cells") <- tibble::tibble(
    row = as.integer(unlist(missing_rows, use.names = FALSE)),
    feature = rep(selected, times = lengths(missing_rows))
  )
  data
}
