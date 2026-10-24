#' Oxford-Comma List With a Fallback for Empty Input
#'
#' Joins a vector into prose ("A", "A and B", "A, B, and C"). Unlike `knitr::combine_words()`, an empty
#' input returns `none` rather than `""`, so narrative text never ends up with a hole in it.
#'
#' @param x A vector (converted to character). `NA` entries are dropped.
#' @param none Text returned when nothing is left.
#' @param and The joining word (default `"and"`; use `"or"` for alternatives). Surrounding spaces are ignored.
#' @return A single string.
#' @seealso [words()], [pluralize()]
#' @export
#' @examples
#' words_list(c("age", "sex", "BMI"))
#' words_list(character())
#' words_list(c("a", "b"), and = "or")
words_list <- function(x, none = "none", and = "and") {
  if (!is.character(and) || length(and) != 1L || is.na(and) || !nzchar(trimws(and))) {
    stop("`and` must be a single word.", call. = FALSE)
  }
  x <- as.character(x)
  x <- x[!is.na(x)]
  and <- trimws(and)
  n <- length(x)
  if (n == 0L) return(none)
  if (n == 1L) return(x)
  if (n == 2L) return(paste(x[1], and, x[2]))
  paste0(paste(x[-n], collapse = ", "), ", ", and, " ", x[n])
}

#' Count and Word With the Right Plural
#'
#' `pluralize(1, "model")` is `"1 model"`, `pluralize(3, "model")` is `"3 models"`. Vectorised over `n`.
#'
#' Only `n == 1` is singular, so 0, 2, 1.5 and negative numbers take the plural (`"0 models"`).
#' The default plural adds "s"; give `plural` for other words (`"child"`, `"children"`).
#' `NA` gives `NA`.
#'
#' @param n Number(s).
#' @param singular The singular word.
#' @param plural The plural word (default: `singular` plus "s").
#' @param big_mark Thousands separator for the number (default none).
#' @return Character vector.
#' @seealso [words_list()]
#' @export
#' @examples
#' pluralize(c(0, 1, 2), "patient")
#' pluralize(2, "child", "children")
pluralize <- function(n, singular, plural = NULL, big_mark = "") {
  if (!is.numeric(n)) stop("`n` must be numeric.", call. = FALSE)
  if (!is.character(singular) || length(singular) != 1L || is.na(singular)) {
    stop("`singular` must be a single string.", call. = FALSE)
  }
  if (is.null(plural)) plural <- paste0(singular, "s")
  num <- vapply(n, function(v) format(v, big.mark = big_mark, trim = TRUE, scientific = FALSE), "")
  out <- paste(num, ifelse(!is.na(n) & n == 1, singular, plural))
  out[is.na(n)] <- NA_character_
  out
}
