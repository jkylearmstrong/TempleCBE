#' Generate Excel-Compatible Column Names
#'
#' Formats column names into sanitized, human-readable strings suitable for Excel output headers.
#'
#' @param names A character vector of column names.
#' @return Sanitized character vector.
#' @export
#' @examples
#' make_excel_names(c("patient_id", "body_mass_index"))
make_excel_names <- function(names) {
  if (!is.character(names)) names <- as.character(names)
  names <- gsub("_", " ", names)
  names <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", names, perl = TRUE)
  names
}
