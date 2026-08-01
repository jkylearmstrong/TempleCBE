#' Clean and Standardize Variable Names
#'
#' Converts variable/column names to lower snake_case and removes invalid special characters.
#'
#' @param data A data frame, tibble, or character vector of names.
#' @return Cleaned data frame or character vector.
#' @export
#' @examples
#' clean_names(c("First Name", "ZIP Code", "Total ($)"))
clean_names <- function(data) {
  if (is.character(data)) {
    return(janitor::make_clean_names(data))
  } else if (is.data.frame(data)) {
    return(janitor::clean_names(data))
  } else {
    stop("Input must be a character vector or data frame.")
  }
}

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
