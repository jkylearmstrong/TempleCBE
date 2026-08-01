#' Pattern Matching and Infix Operator Utilities
#'
#' Useful infix operators for string matching and logical negation.
#'
#' @param x Vector to test.
#' @param table Vector of values to test against (for \code{\%notin\%}).
#' @param pattern Pattern to match (for \code{\%like\%}, \code{\%flike\%}, \code{\%ilike\%}).
#' @return A logical vector.
#' @name infix_helpers
#' @export
`%notin%` <- function(x, table) {
  !(x %in% table)
}

#' @rdname infix_helpers
#' @export
`%like%` <- function(x, pattern) {
  grepl(pattern, x)
}

#' @rdname infix_helpers
#' @export
`%ilike%` <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE)
}

#' @rdname infix_helpers
#' @export
`%flike%` <- function(x, pattern) {
  grepl(pattern, x, fixed = TRUE)
}
