#' Read Excel Data With Multi-Row Column Headers
#'
#' Variant of \code{\link[readxl]{read_excel}} for sheets where a column's
#' name is split across multiple header rows — the rows are concatenated in
#' order, joined with \code{" | "}.
#'
#' @param path Path to the \code{.xls}/\code{.xlsx} file.
#' @param n_header_rows Number of header rows in the sheet.
#' @param ... Additional arguments passed to \code{\link[readxl]{read_excel}}.
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2)
#' }
read_excel_multiple_headers <- function(path, n_header_rows, ...) {
  concatenate_values <- function(x) paste(stats::na.omit(x), collapse = " | ")

  col_data_raw <- readxl::read_excel(path = path, n_max = n_header_rows, col_names = FALSE, ...)
  col_data <- dplyr::summarise(col_data_raw, dplyr::across(dplyr::everything(), concatenate_values))
  my_colnames <- tidyr::pivot_longer(col_data, dplyr::everything()) |> dplyr::pull("value")

  data <- readxl::read_excel(path = path, col_names = FALSE, skip = n_header_rows, ...)
  colnames(data) <- my_colnames
  data
}
