#' Variable Label and Excel Header Helpers
#'
#' Utilities for converting raw or snake_case column names to human-readable
#' display names, formatting Excel-safe column headers, and copying variable
#' labels across data frames.
#'
#' @name label_helpers
NULL

#' Convert Raw Variable Names to Human-Readable Display Labels
#'
#' Converts snake_case, camelCase, or dot-delimited variable names into clean,
#' title-cased labels suitable for tables and figures. Preserves common clinical
#' acronyms (e.g. BMI, ID, IBS, HR, SD, IQR, CBE, RNA, DNA).
#'
#' @param x Character vector of variable names.
#' @param acronyms Optional character vector of terms to keep uppercase.
#' @return Character vector of formatted display labels.
#' @export
#' @examples
#' pretty_name(c("patient_bmi", "follow_up_time", "hr_ratio_sd"))
pretty_name <- function(x, acronyms = c("BMI", "ID", "IBS", "HR", "SD", "IQR", "CBE", "RNA", "DNA", "CV", "AUC", "ROC", "IPCW")) {
  if (is.null(x)) return(character())
  x <- as.character(x)

  # Replace underscores, dots, hyphens with spaces
  clean <- gsub("[._-]+", " ", x)
  # Insert space before camelCase capitals
  clean <- gsub("([a-z])([A-Z])", "\\1 \\2", clean)
  # Trim excess whitespace
  clean <- trimws(gsub("\\s+", " ", clean))

  # Title case
  res <- tools::toTitleCase(clean)

  # Restore acronyms
  if (length(acronyms) > 0) {
    for (acr in acronyms) {
      pattern <- paste0("\\b", acr, "\\b")
      res <- gsub(pattern, acr, res, ignore.case = TRUE)
    }
  }

  res
}

#' Sanitize Column Names for Excel Worksheets
#'
#' Replaces characters forbidden by Excel (e.g. `\ / ? * : [ ]`) with underscores
#' or spaces, trims leading/trailing spaces, and optionally truncates names.
#'
#' @param names Character vector of proposed column or sheet names.
#' @param max_char Optional integer maximum character length (e.g. 31 for Excel sheet names).
#' @param replacement Replacement character for invalid symbols (default `"_"`).
#' @return Character vector of Excel-safe names.
#' @export
#' @examples
#' excel_colnames(c("Patient/Subject [ID]", "Cost ($/hr)?"))
excel_colnames <- function(names, max_char = NULL, replacement = "_") {
  if (is.null(names)) return(character())
  names <- as.character(names)

  # Forbidden Excel characters: \ / ? * : [ ]
  cleaned <- gsub("[\\\\/?*:[\\]]", replacement, names)
  # Remove duplicate replacement characters
  if (nzchar(replacement)) {
    pattern <- paste0(replacement, "+")
    cleaned <- gsub(pattern, replacement, cleaned)
  }
  cleaned <- trimws(cleaned)

  if (!is.null(max_char) && is.numeric(max_char) && max_char > 0) {
    cleaned <- substr(cleaned, 1L, as.integer(max_char))
  }

  # Ensure names are non-empty
  empty_idx <- which(is.na(cleaned) | cleaned == "")
  if (length(empty_idx) > 0) {
    cleaned[empty_idx] <- paste0("Col_", empty_idx)
  }

  cleaned
}

#' Copy Variable Labels and Attributes Across Data Frames
#'
#' Transfers column-level attributes (such as `label`, `labels`, and `format.sas`)
#' from a source data frame (`from`) to matching columns in a target data frame (`to`).
#'
#' @param from Source data frame containing labelled variables.
#' @param to Target data frame to receive variable attributes.
#' @param attrs Character vector of attribute names to copy (default `c("label", "labels", "format.sas")`).
#' @return Target data frame `to` with transferred attributes.
#' @export
#' @examples
#' df1 <- data.frame(age = 50, bmi = 24.5)
#' attr(df1$age, "label") <- "Age at Enrollment (years)"
#' df2 <- data.frame(age = 60, bmi = 28.0)
#' df2 <- copy_var_labels(df1, df2)
#' attr(df2$age, "label")
copy_var_labels <- function(from, to, attrs = c("label", "labels", "format.sas")) {
  if (!is.data.frame(from) || !is.data.frame(to)) {
    stop("Both `from` and `to` must be data frames.", call. = FALSE)
  }

  common_cols <- intersect(names(from), names(to))
  for (col in common_cols) {
    for (att in attrs) {
      val <- attr(from[[col]], att, exact = TRUE)
      if (!is.null(val)) {
        attr(to[[col]], att) <- val
      }
    }
  }

  to
}
