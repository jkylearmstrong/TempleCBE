# Fixtures shared by the review-extractor tests that build Word documents and read the
# workbooks back. A verbatim copy of the helpers at the top of test-cbe_review_merge.R, so
# that other test files can use them (a test file's own definitions are not visible to others).


A6_W <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

# Write one UTF-8 part of a package into directory `td`.
a6_write_part <- function(td, name, content) {
  path <- file.path(td, name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(enc2utf8(content)), path)
}

# Build a .docx from XML strings. `body` is the inside of <w:body>; `comments`
# the inside of <w:comments> (NULL = no comments part); `parts` any extra
# parts (named by their path in the package); `document_xml` replaces the whole
# document part (for malformed documents); `document = FALSE` leaves the
# document part out.
a6_docx <- function(path, body = '<w:p><w:r><w:t>Hello</w:t></w:r></w:p>',
                    comments = NULL, parts = list(), document_xml = NULL,
                    document = TRUE) {
  td <- tempfile(pattern = "a6_docx_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  a6_write_part(td, "[Content_Types].xml", paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ))
  if (document) {
    if (is.null(document_xml)) {
      document_xml <- paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<w:document xmlns:w="', A6_W, '"><w:body>', body, '</w:body></w:document>'
      )
    }
    a6_write_part(td, "word/document.xml", document_xml)
  }
  if (!is.null(comments)) {
    a6_write_part(td, "word/comments.xml", paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<w:comments xmlns:w="', A6_W, '">', paste0(comments, collapse = ""), '</w:comments>'
    ))
  }
  for (nm in names(parts)) a6_write_part(td, nm, parts[[nm]])

  if (file.exists(path)) unlink(path)
  # top-level entries only: zipr() recurses into "word" and keeps its path
  zip::zipr(path, files = list.files(td, all.files = TRUE, no.. = TRUE), root = td)
  invisible(path)
}

# <w:comment> with one paragraph per element of `text`
a6_comment <- function(id, text, author = "Ann", date = "2026-01-01T10:00:00Z") {
  paras <- paste0('<w:p><w:r><w:t>', text, '</w:t></w:r></w:p>', collapse = "")
  sprintf('<w:comment w:id="%s" w:author="%s" w:date="%s">%s</w:comment>', id, author, date, paras)
}

# A paragraph whose text is covered by comment range `id`
a6_commented_para <- function(id, text) {
  sprintf('<w:p><w:commentRangeStart w:id="%s"/><w:r><w:t>%s</w:t></w:r><w:commentRangeEnd w:id="%s"/></w:p>', id, text, id)
}

# A paragraph with plain text, optionally followed by a tracked insertion
a6_para <- function(text, ins = NULL, author = "Cat", date = "2026-01-02T10:00:00Z") {
  extra <- if (is.null(ins)) "" else sprintf(
    '<w:ins w:id="1" w:author="%s" w:date="%s"><w:r><w:t>%s</w:t></w:r></w:ins>', author, date, ins
  )
  sprintf('<w:p><w:r><w:t>%s</w:t></w:r>%s</w:p>', text, extra)
}

# Round-trip reader: one sheet of a workbook as a character tibble
a6_sheet <- function(path, sheet) {
  suppressMessages(readxl::read_excel(path, sheet = sheet, col_types = "text"))
}

# Write cells of the row whose `match_col` equals `match_val`
a6_set_cells <- function(xlsx, sheet, match_col, match_val, values) {
  wb <- openxlsx::loadWorkbook(xlsx)
  df <- openxlsx::readWorkbook(wb, sheet, colNames = TRUE)
  row_i <- which(df[[match_col]] == match_val)[1] + 1L
  for (nm in names(values)) {
    openxlsx::writeData(wb, sheet, values[[nm]], startCol = which(names(df) == nm), startRow = row_i)
  }
  openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)
}

# Run the extractor on `dir` with the given reviewers; options restored on exit
a6_options <- function(fork = c("rev_a", "rev_b"), signoff = character(0), mode = "columns",
                       env = parent.frame()) {
  old <- options(
    review.fork_reviewers = fork,
    review.signoff_reviewers = signoff,
    review.documents_sheet_mode = mode
  )
  withr::defer(options(old), envir = env)
  invisible(old)
}

a6_dir <- function(env = parent.frame()) {
  d <- tempfile(pattern = "a6_review_")
  dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}

