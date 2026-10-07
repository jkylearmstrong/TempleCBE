# Shared fixtures for the review-extractor privacy / configuration tests.
# Everything here is invented: generic reviewer ids, stage names and text.

#' Write a minimal Word-style .docx with comments and tracked insertions
#'
#' @param path Target .docx path.
#' @param paragraphs Character vector, one entry per paragraph.
#' @param comments List of \code{list(id, author, text, para)}: a comment anchored
#'   on paragraph number \code{para}.
#' @param insertions List of \code{list(author, text, para)}: a tracked insertion
#'   appended to paragraph number \code{para}.
#' @noRd
build_review_docx <- function(path,
                              paragraphs = "Flagged text.",
                              comments = list(),
                              insertions = list()) {
  w_ns <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
  }

  para_xml <- vapply(seq_along(paragraphs), function(i) {
    starts <- ends <- ins <- ""
    for (cm in comments) {
      if (identical(as.integer(cm$para), i)) {
        starts <- paste0(starts, sprintf('<w:commentRangeStart w:id="%s"/>', cm$id))
        ends <- paste0(ends, sprintf('<w:commentRangeEnd w:id="%s"/>', cm$id))
      }
    }
    for (rv in insertions) {
      if (identical(as.integer(rv$para), i)) {
        ins <- paste0(ins, sprintf(
          '<w:ins w:id="90%d" w:author="%s" w:date="2026-01-02T10:00:00Z"><w:r><w:t>%s</w:t></w:r></w:ins>',
          i, esc(rv$author), esc(rv$text)
        ))
      }
    }
    paste0("<w:p>", starts, "<w:r><w:t>", esc(paragraphs[i]), "</w:t></w:r>", ends, ins, "</w:p>")
  }, character(1))

  document_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="', w_ns, '"><w:body>', paste(para_xml, collapse = ""), "</w:body></w:document>"
  )
  comments_xml <- NULL
  if (length(comments) > 0) {
    items <- vapply(comments, function(cm) {
      sprintf(
        '<w:comment w:id="%s" w:author="%s" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>%s</w:t></w:r></w:p></w:comment>',
        cm$id, esc(cm$author), esc(cm$text)
      )
    }, character(1))
    comments_xml <- paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
      '<w:comments xmlns:w="', w_ns, '">', paste(items, collapse = ""), "</w:comments>"
    )
  }

  td <- tempfile(pattern = "review_docx_")
  dir.create(file.path(td, "word"), recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ), file.path(td, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(document_xml, file.path(td, "word", "document.xml"), useBytes = TRUE)
  if (!is.null(comments_xml)) writeLines(comments_xml, file.path(td, "word", "comments.xml"), useBytes = TRUE)

  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  old_wd <- setwd(td)
  on.exit(setwd(old_wd), add = TRUE)
  if (file.exists(target)) unlink(target)
  utils::zip(target, files = list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE), flags = "-r9Xq")
  invisible(path)
}

#' Every text the workbook holds, as one string per xlsx part
#'
#' Unzips the workbook and reads each XML part (cell strings, formulas, sheet
#' names, defined names, styles ...) raw, so a leak cannot hide in a hidden
#' column, a hidden sheet or a formula.
#' @noRd
xlsx_raw_parts <- function(xlsx_path) {
  td <- tempfile(pattern = "xlsx_parts_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(xlsx_path, exdir = td)
  parts <- list.files(td, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  stats::setNames(
    lapply(parts, function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
    substring(parts, nchar(td) + 2L)
  )
}

#' Options that the review tests change, saved so a test can restore them
#' @noRd
review_options_reset <- function() {
  options(
    review.fork_reviewers = NULL,
    review.signoff_reviewers = NULL,
    review.documents_sheet_mode = NULL,
    review.csv_reviewer_columns = NULL,
    review.pipeline_catalog = NULL,
    review.stem_aliases = NULL,
    review.analysis_dir = NULL,
    review.input_dirs = NULL
  )
}
