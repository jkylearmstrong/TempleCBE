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
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("`na.rm` must be a single logical (TRUE or FALSE).", call. = FALSE)
  }
  dplyr::if_else(na.rm,
                 stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
                 stats::sd(x, na.rm = FALSE) / sqrt(length(x))
  )
}
