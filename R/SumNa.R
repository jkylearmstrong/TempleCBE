#' Count Total Missing (NA) Values
#'
#' Calculates the total number of `NA` values across a vector, matrix, or data frame.
#'
#' @param x A vector, matrix, or data frame.
#' @return An integer representing the total count of missing values.
#' @export
#' @examples
#' SumNa(c(1, 2, NA, 4, NA))
#' SumNa(data.frame(a = c(1, NA), b = c(NA, 2)))
SumNa <- function(x) {
  sum(is.na(x))
}
