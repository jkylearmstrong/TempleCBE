#' Read Every Sheet of an Excel Workbook
#'
#' Variant of \code{\link[readxl]{read_excel}} that reads every sheet in a
#' workbook into a named list of tibbles, named after each sheet.
#'
#' @param path Path to the \code{.xls}/\code{.xlsx} file.
#' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}.
#' @return A named list of tibbles, one per sheet.
#' @export
#' @examples
#' \dontrun{
#' read_workbook("workbook.xlsx")
#' }
read_workbook <- function(path, ...) {
  sheets <- readxl::excel_sheets(path)
  names(sheets) <- sheets
  purrr::map(sheets, \(sheet) readxl::read_excel(path = path, sheet = sheet, ...))
}
