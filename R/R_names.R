#' Clean Column Names, Preserving Originals as Labels
#'
#' Runs \code{\link[janitor]{clean_names}} on \code{df} and stores each
#' column's original name as a \pkg{labelled} variable label, so the
#' human-readable original isn't lost when the column name itself becomes
#' machine-friendly.
#'
#' @param df A data frame or tibble.
#' @param ... Passed to \code{clean_names_keep_labels()}.
#' @return A tibble with cleaned column names and the original names stored
#'   as variable labels (see \code{\link[labelled]{var_label}}).
#' @export
#' @examples
#' df <- tibble::tibble(
#'   `name with spaces` = 1:10,
#'   `special * characters` = LETTERS[1:10]
#' )
#' clean_names_keep_labels(df)
clean_names_keep_labels <- function(df) {
  old_names <- colnames(df)
  df <- janitor::clean_names(df)
  labelled::var_label(df) <- old_names
  df
}

#' @rdname clean_names_keep_labels
#' @export
R_names <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "R_names()", "clean_names_keep_labels()")
  clean_names_keep_labels(...)
}
