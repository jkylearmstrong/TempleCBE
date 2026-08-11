#' Count Total Missing (NA) Values
#'
#' Calculates the total number of missing values across a vector, matrix, or
#' data frame, optionally treating additional values (e.g. \code{"NA"} as a
#' literal string, or \code{""}) as missing too.
#'
#' @param x A vector, matrix, or data frame.
#' @param na_list Optional vector of additional values to treat as missing,
#'   beyond actual \code{NA}.
#' @return An integer representing the total count of missing values.
#' @export
#' @examples
#' SumNa(c(1, 2, NA, 4, NA))
#' SumNa(data.frame(a = c(1, NA), b = c(NA, 2)))
#' SumNa(c(1, NA, "NA", 4), na_list = "NA")
SumNa <- function(x, na_list = NULL) {
  if (is.null(na_list)) sum(is.na(x)) else sum(is.na(x) | x %in% na_list)
}
