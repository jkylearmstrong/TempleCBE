#' Write Data Frame to Excel (.xlsx) File
#'
#' A non-masking wrapper around [writexl::write_xlsx()] to write data frames
#' or lists of data frames to Excel files.
#'
#' @param x A data frame or named list of data frames that will be written to Excel sheets.
#' @param path Path to the output Excel `.xlsx` file.
#' @param ... Additional arguments passed to [writexl::write_xlsx()].
#' @return The normalized path to the written file, invisibly.
#' @seealso [writexl::write_xlsx()], [write_database_to_excel()]
#' @export
#' @examples
#' \donttest{
#' tmp <- tempfile(fileext = ".xlsx")
#' write_xlsx_file(mtcars, tmp)
#' unlink(tmp)
#' }
write_xlsx_file <- function(x, path = tempfile(fileext = ".xlsx"), ...) {
  rlang::check_installed("writexl", reason = "for write_xlsx_file().")
  writexl::write_xlsx(x = x, path = path, ...)
}

#' Write Data Frame to Excel (.xlsx)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `write_xlsx()` in TempleCBE was a re-export of [writexl::write_xlsx()] and is now
#' deprecated to avoid masking conflicts when both packages are loaded.
#' Scheduled for complete removal in TempleCBE 0.6.0 (Q1 2027).
#' Call [write_xlsx_file()] or [writexl::write_xlsx()] directly instead.
#'
#' @param ... Passed to [writexl::write_xlsx()].
#' @export
write_xlsx <- function(...) {
  lifecycle::deprecate_warn(
    "0.5.8", "write_xlsx()", "write_xlsx_file()",
    details = "Scheduled for complete removal in TempleCBE 0.6.0 (Q1 2027) to avoid masking writexl::write_xlsx()."
  )
  writexl::write_xlsx(...)
}

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
