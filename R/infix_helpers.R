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
#' @param ... Passed to \code{match_like()}.
#' @return A logical vector.
#' @name infix_helpers
NULL

#' @rdname infix_helpers
#' @export
match_like <- function(vector, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE) {
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
like <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "like()", "match_like()", details = "Scheduled for complete removal in TempleCBE 0.7.0 to avoid masking data.table::like().")
  match_like(...)
}

#' @rdname infix_helpers
#' @export
`%like%` <- function(vector, pattern) {
  warning("`%like%` was deprecated in TempleCBE 0.5.8. Please use `match_like()` instead. Scheduled for complete removal in TempleCBE 0.7.0.", call. = FALSE)
  match_like(vector, pattern)
}

#' @rdname infix_helpers
#' @export
ilike <- function(vector, pattern) match_like(vector, pattern, ignore.case = TRUE)

#' @rdname infix_helpers
#' @export
`%ilike%` <- function(vector, pattern) match_like(vector, pattern, ignore.case = TRUE)

#' @rdname infix_helpers
#' @export
flike <- function(vector, pattern) match_like(vector, pattern, fixed = TRUE)

#' @rdname infix_helpers
#' @export
`%flike%` <- function(vector, pattern) match_like(vector, pattern, fixed = TRUE)

#' @rdname infix_helpers
#' @export
plike <- function(vector, pattern) match_like(vector, pattern, perl = TRUE)

#' @rdname infix_helpers
#' @export
`%plike%` <- function(vector, pattern) match_like(vector, pattern, perl = TRUE)

#' @rdname infix_helpers
#' @export
notin <- function(x, table) !(x %in% table)

#' @rdname infix_helpers
#' @export
`%!in%` <- function(x, table) notin(x, table)

# Unexported in TempleCBE 0.5.9 to avoid masking base R 4.6 `%notin%`. Retained as internal shim.
`%notin%` <- function(x, table) {
  warning("`%notin%` was deprecated in TempleCBE 0.5.8 and unexported in 0.5.9. Please use `%!`in`%` or `!(x %in% table)` instead. Scheduled for complete removal in TempleCBE 0.7.0.", call. = FALSE)
  notin(x, table)
}
