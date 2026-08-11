#' Pattern Matching and Logical-Negation Infix Operators
#'
#' A small family of \code{grepl()}/\code{\%in\%} wrappers as base functions
#' and matching infix operators.
#'
#' @param x,vector A vector to test.
#' @param table Vector of values to test against (for \code{notin()}/\code{\%notin\%}/\code{\%!in\%}).
#' @param pattern Pattern to match (for the \code{like} family).
#' @param ignore.case,fixed,perl Logical flags controlling how \code{pattern}
#'   is matched, as in \code{\link{grepl}}.
#' @return A logical vector.
#' @name infix_helpers
NULL

#' @rdname infix_helpers
#' @export
like <- function(vector, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE) {
  if (is.factor(vector)) {
    ret <- grepl(pattern, levels(vector), ignore.case = ignore.case, fixed = fixed, perl = perl)[vector]
    ret[is.na(ret)] <- FALSE
    ret
  } else {
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed, perl = perl)
  }
}

#' @rdname infix_helpers
#' @export
`%like%` <- function(vector, pattern) like(vector, pattern)

#' @rdname infix_helpers
#' @export
ilike <- function(vector, pattern) like(vector, pattern, ignore.case = TRUE)

#' @rdname infix_helpers
#' @export
`%ilike%` <- function(vector, pattern) like(vector, pattern, ignore.case = TRUE)

#' @rdname infix_helpers
#' @export
flike <- function(vector, pattern) like(vector, pattern, fixed = TRUE)

#' @rdname infix_helpers
#' @export
`%flike%` <- function(vector, pattern) like(vector, pattern, fixed = TRUE)

#' @rdname infix_helpers
#' @export
plike <- function(vector, pattern) like(vector, pattern, perl = TRUE)

#' @rdname infix_helpers
#' @export
`%plike%` <- function(vector, pattern) like(vector, pattern, perl = TRUE)

#' @rdname infix_helpers
#' @export
notin <- function(x, table) !(x %in% table)

#' @rdname infix_helpers
#' @export
`%!in%` <- function(x, table) notin(x, table)

#' @rdname infix_helpers
#' @export
`%notin%` <- function(x, table) notin(x, table)
