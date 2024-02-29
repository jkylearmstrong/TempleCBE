#' Count the number of missing elements of a vector
#'
#' Count the number of missing elements of a vector,
#' allows user to define what should count as \code{NA} in R
#' The function is vectorized.
#'
#' @param col column or vector of data
#' @param na_list list of elements to allow SumNa to think of as NA
#'
#' @return numeric value representing number of missing values
#'
#' @examples
#' data <- data.frame(
#'    col1 = c(1, NA, 3, "NA"),
#'    col2 = c("A", "B", "C", "D")
#'    )
#' SumNa(data$col1)
#' SumNa(data$col1, na_list = c("NA"))
#'
#' @importFrom dplyr if_else
#'
#' @export SumNa


SumNa <- function(col, na_list = NULL) {
  if_else(
    is.null(na_list),
    sum(is.na(col)), # only R version of NA
    sum(is.na(col) | col %in% na_list)) # allows user to pass in list of other NA values
}
