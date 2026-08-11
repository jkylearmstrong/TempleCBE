#' Build a Table of Contents from a SAS-Generated PDF
#'
#' Scans a PDF (as produced by SAS reporting output) for table titles and
#' writes a Table-of-Contents \code{.rtf} file alongside it, prefixed
#' \code{TOC_}.
#'
#' @param input Path to the input PDF.
#' @param collapse If \code{FALSE} (default), every occurrence of a title is
#'   listed; if \code{TRUE}, only the first in a numbered sequence is kept.
#' @param top_margin_height How far down the page (in PDF text-position
#'   units) to look for titles.
#' @return Invisibly, a message string naming the output file path.
#' @export
#' @examples
#' \dontrun{
#' create_toc_from_sas_pdf("path/to/report.pdf", collapse = TRUE)
#' }
create_toc_from_sas_pdf <- function(input, collapse = FALSE, top_margin_height = 60) {
  dir_path <- dirname(input)
  base <- basename(input)
  file_extension <- tools::file_ext(input)
  if (tolower(file_extension) != "pdf") stop("only PDF files for now")

  input_base <- sub(paste0("\\.", file_extension, "$"), "", base)
  output_base <- paste0("TOC_", input_base)

  extract_top_titles <- function(page_data) {
    top_text <- page_data |>
      dplyr::filter(.data$y < top_margin_height) |>
      dplyr::arrange(.data$y, .data$x) |>
      dplyr::pull("text")
    paste(top_text, collapse = " ")
  }

  pdf_data_info <- pdftools::pdf_data(input)
  top_titles <- vapply(pdf_data_info, extract_top_titles, character(1))
  top_titles <- top_titles[top_titles != "" & !grepl("^\\s*$", top_titles)]

  is_duplicate <- if (isTRUE(collapse)) {
    function(title, seen_titles) {
      title_base <- gsub("\\s*-?\\s*\\d+$", "", trimws(tolower(title)))
      seen_bases <- gsub("\\s*-?\\s*\\d+$", "", trimws(tolower(seen_titles)))
      title_base %in% seen_bases
    }
  } else {
    function(title, seen_titles) trimws(tolower(title)) %in% trimws(tolower(seen_titles))
  }

  seen_titles <- character(0)
  filtered_titles <- character(0)
  page_numbers <- integer(0)

  for (i in seq_along(top_titles)) {
    title <- top_titles[i]
    clean_title <- gsub("^[0-9]+\\s*", "", title)
    if (!grepl("Title\\s*Page", clean_title, ignore.case = TRUE) && !is_duplicate(clean_title, seen_titles)) {
      filtered_titles <- c(filtered_titles, clean_title)
      page_numbers <- c(page_numbers, i)
      seen_titles <- c(seen_titles, clean_title)
    }
  }

  output_rtf <- file.path(dir_path, paste0(output_base, ".rtf"))
  table_data <- data.frame(Title = filtered_titles, Page = format(page_numbers, width = 5))
  col_widths <- c(5.5, 1.0)

  table_data |>
    r2rtf::rtf_page(width = 8.5, height = 11, margin = c(1, 1, 1, 1, 0.5, 0.5), col_width = 6.5) |>
    r2rtf::rtf_title("Table of Contents:") |>
    r2rtf::rtf_colheader(colheader = "Title | Page", col_rel_width = col_widths, text_justification = c("l", "r")) |>
    r2rtf::rtf_body(col_rel_width = col_widths, text_justification = c("l", "r"), border_top = "", border_bottom = "") |>
    r2rtf::rtf_encode() |>
    r2rtf::write_rtf(output_rtf)

  invisible(paste0("TOC saved as RTF: ", output_rtf))
}
