#' Convert a PDF's Text to Rich Text Format (RTF)
#'
#' Extracts the text layer of a PDF with [pdftools::pdf_text()] and writes it
#' to an RTF file in a monospaced font, one RTF page per PDF page, so the
#' column alignment of text tables (such as SAS listings) is kept.
#'
#' Only text is converted: images, fonts, and vector graphics are not. A
#' scanned PDF with no text layer gives empty pages. Non-ASCII characters are
#' written as RTF Unicode escapes, so the file is plain ASCII and opens the
#' same in any locale.
#'
#' @param pdf Path to the input `.pdf` file.
#' @param rtf Path of the `.rtf` file to write. Defaults to `pdf` with its
#'   extension replaced by `.rtf`.
#' @param font_size Font size in points.
#' @param overwrite If `FALSE`, error when `rtf` already exists.
#' @return The path to `rtf`, invisibly.
#' @seealso [create_toc_from_sas_pdf()]
#' @export
#' @examples
#' pdf <- system.file("templates", "example.pdf", package = "TempleCBE")
#' rtf <- pdf_to_rtf(pdf, tempfile(fileext = ".rtf"))
#' readLines(rtf, n = 3)
pdf_to_rtf <- function(pdf,
                       rtf = sub("\\.pdf$", ".rtf", pdf, ignore.case = TRUE),
                       font_size = 10,
                       overwrite = TRUE) {
  if (!is.character(pdf) || length(pdf) != 1 || is.na(pdf) || !file.exists(pdf)) {
    stop("`pdf` must be the path of an existing file.", call. = FALSE)
  }
  if (tolower(tools::file_ext(pdf)) != "pdf") {
    stop("`pdf` must be a .pdf file: ", pdf, call. = FALSE)
  }
  if (!is.character(rtf) || length(rtf) != 1 || is.na(rtf) || identical(rtf, pdf)) {
    stop("`rtf` must be a single path different from `pdf`.", call. = FALSE)
  }
  if (!is.numeric(font_size) || length(font_size) != 1 || !(font_size > 0)) {
    stop("`font_size` must be a single positive number.", call. = FALSE)
  }
  if (file.exists(rtf) && !isTRUE(overwrite)) {
    stop("`rtf` already exists and `overwrite = FALSE`: ", rtf, call. = FALSE)
  }

  pages <- pdftools::pdf_text(pdf)
  body <- paste(vapply(pages, rtf_escape_text, character(1), USE.NAMES = FALSE), collapse = "\n\\page\n")
  header <- paste0(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0\n",
    "{\\fonttbl{\\f0\\fmodern\\fcharset0 Courier New;}}\n",
    "\\viewkind4\\uc1\\pard\\f0\\fs", round(font_size * 2), "\n"
  )

  writeLines(paste0(header, body, "\n}"), rtf, useBytes = TRUE)
  invisible(rtf)
}

#' Escape Plain Text for an RTF Body
#'
#' Escapes the RTF control characters `\`, `{`, and `}`; turns tabs into
#' `\tab` and line breaks into `\par`; drops form feeds and trailing blank
#' lines; and writes every non-ASCII character as a `\uN?` escape (a UTF-16
#' surrogate pair above U+FFFF), as the RTF 1.9 specification requires.
#'
#' @param text A single string.
#' @return A single ASCII string.
#' @keywords internal
#' @noRd
rtf_escape_text <- function(text) {
  text <- iconv(enc2utf8(text), "UTF-8", "UTF-8", sub = "?")
  text <- gsub("\r\n?", "\n", text)
  text <- gsub("\f", "", text, fixed = TRUE)
  text <- sub("\n+$", "", text)
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("{", "\\{", text, fixed = TRUE)
  text <- gsub("}", "\\}", text, fixed = TRUE)
  text <- gsub("\t", "\\tab ", text, fixed = TRUE)
  text <- gsub("\n", "\\par\n", text, fixed = TRUE)

  codepoints <- utf8ToInt(text)
  non_ascii <- which(codepoints > 127)
  if (!length(non_ascii)) {
    return(text)
  }

  # RTF \u takes a signed 16-bit value.
  signed16 <- function(u) ifelse(u > 32767, u - 65536, u)
  cp <- codepoints[non_ascii]
  escaped <- sprintf("\\u%d?", signed16(cp))
  astral <- cp > 0xFFFF
  if (any(astral)) {
    offset <- cp[astral] - 0x10000
    escaped[astral] <- sprintf(
      "\\u%d?\\u%d?",
      signed16(0xD800 + offset %/% 0x400),
      signed16(0xDC00 + offset %% 0x400)
    )
  }

  pieces <- intToUtf8(codepoints, multiple = TRUE)
  pieces[non_ascii] <- escaped
  paste(pieces, collapse = "")
}
