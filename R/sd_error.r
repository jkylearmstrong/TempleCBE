#' Compute standard error
#'
#' @param x vector of data
#' @param na.rm should NA values be removed, TRUE or FALSE, default is FALSE
#'
#' @return numeric value representing standard error
#'
#' @examples
#' std_error(mtcars$mpg)
#'
#' @importFrom dplyr if_else
#' @importFrom stats sd
#'
#' @export
std_error <- function(x, na.rm = FALSE) {
  dplyr::if_else(na.rm,
                 stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
                 stats::sd(x, na.rm = FALSE) / sqrt(length(x))
  )
}

#' @rdname std_error
#' @export
sd.error <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "sd.error()", "std_error()")
  std_error(...)
}
