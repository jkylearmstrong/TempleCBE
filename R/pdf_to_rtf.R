#' Convert a PDF to Rich Text Format (RTF)
#'
#' Extracts text from a PDF via \code{\link[pdftools]{pdf_text}} and writes
#' it to a portable \code{.rtf} file.
#'
#' @param path_To_PDF_File Path to the input \code{.pdf} file.
#' @param path_To_write_RTF_File Path to write the converted \code{.rtf} file to.
#' @return The normalized path to the created RTF file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' pdf_to_rtf("report.pdf", "report.rtf")
#' }
pdf_to_rtf <- function(path_To_PDF_File, path_To_write_RTF_File) {
  if (!file.exists(path_To_PDF_File)) {
    stop("Input PDF file does not exist: ", path_To_PDF_File)
  }

  pages <- pdftools::pdf_text(path_To_PDF_File)

  escaped_pages <- vapply(pages, function(page) {
    page <- gsub("\\\\", "\\\\\\\\", page)
    page <- gsub("\\{", "\\\\{", page)
    page <- gsub("\\}", "\\\\}", page)
    gsub("\n", "\\\\par\n", page)
  }, character(1))

  rtf_body <- paste(escaped_pages, collapse = "\n\\\\page\n")
  rtf_content <- paste0(
    "{\\rtf1\\ansi\\deff0\n",
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}\n",
    "\\viewkind4\\uc1\\pard\\f0\\fs22\n",
    rtf_body,
    "\n}"
  )

  writeLines(rtf_content, path_To_write_RTF_File, useBytes = TRUE)
  invisible(normalizePath(path_To_write_RTF_File, mustWork = FALSE))
}
