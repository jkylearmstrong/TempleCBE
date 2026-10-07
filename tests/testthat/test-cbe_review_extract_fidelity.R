# Extraction fidelity and speed of the Word-review extractor (audit chunk A6).
#
# Every fixture is built from the OOXML layout by hand; none of them was written
# by Word, so what these tests show is that the extractor reads that layout, not
# that Word writes it.

W_DECL <- 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"'
W14_DECL <- 'xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"'
MC_DECL <- paste0(
  'xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" ',
  'xmlns:v="urn:schemas-microsoft-com:vml"'
)
XML_PROLOG <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'

# Build a .docx from a body and any extra parts (name -> XML text), zipped with
# the paths the archive needs. Returns the path.
write_test_docx <- function(path, body, comments = NULL, parts = list(), document_ns = "") {
  skip_if_not_installed("zip")
  root <- tempfile("fid_docx_")
  dir.create(file.path(root, "word"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  all_parts <- c(
    list(
      "[Content_Types].xml" = paste0(
        XML_PROLOG,
        '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
        '<Default Extension="xml" ContentType="application/xml"/></Types>'
      ),
      "word/document.xml" = paste0(
        XML_PROLOG, "<w:document ", W_DECL, " ", document_ns, "><w:body>", body, "</w:body></w:document>"
      )
    ),
    if (!is.null(comments)) list("word/comments.xml" = comments),
    parts
  )
  for (nm in names(all_parts)) {
    dest <- file.path(root, nm)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(all_parts[[nm]])), dest)
  }
  if (file.exists(path)) unlink(path)
  # "mirror" keeps word/document.xml; zipr()'s default stores every part at the top level
  zip::zipr(path, files = names(all_parts), root = root, mode = "mirror")
  invisible(path)
}

comments_part <- function(...) {
  paste0(XML_PROLOG, "<w:comments ", W_DECL, " ", W14_DECL, ">", paste0(..., collapse = ""), "</w:comments>")
}

fid_tmp_dir <- function(env = parent.frame()) {
  d <- tempfile("fid_")
  dir.create(d, recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}

# ---- workbook writer -------------------------------------------------------

# Arguments of write_review_tracker_excel() for a small, fully specified table
writer_args <- function(n_comments = 6, n_redlines = 4, typed = TRUE) {
  ids <- c("rev_a", "rev_b")
  files <- rep(c("one.docx", "two.docx"), length.out = n_comments)
  comments <- tibble::tibble(
    file = files,
    comment_id = as.character(seq_len(n_comments)),
    author = "Ann",
    date = "2026-01-01T10:00:00Z",
    comment_text = paste("comment", seq_len(n_comments)),
    selected_text = NA_character_,
    paragraph_number = seq_len(n_comments),
    end_paragraph_number = NA_integer_,
    paragraph_context = "",
    resolved = NA,
    rev_a = rep(c(TRUE, NA), length.out = n_comments),
    rev_a_comment = NA_character_,
    rev_b = FALSE,
    rev_b_comment = "needs work",
    doc_status = "Active",
    pipeline_stage = "",
    is_reply = FALSE,
    reply_to_id = NA_character_,
    duplicate_count = 1L
  )
  if (!typed) comments[] <- lapply(comments, function(x) if (is.character(x)) x else as.character(x))
  redlines <- tibble::tibble(
    file = rep("one.docx", n_redlines),
    paragraph_number = seq_len(n_redlines),
    author = "Ann; Bob",
    date = NA_character_,
    original_text = "old",
    accepted_text = "new",
    is_toc_or_lof = FALSE,
    is_comment = NA,
    resolved = NA,
    rev_a = TRUE, rev_a_comment = NA_character_, rev_b = NA, rev_b_comment = NA_character_,
    doc_status = "Active", pipeline_stage = ""
  )
  list(
    columns = names(comments),
    comments_rows = comments,
    revisions_rows = tibble::tibble(
      file = "one.docx", revision_type = "insertion", author = "Ann", date = NA_character_,
      changed_text = "new", paragraph_number = 1L, paragraph_context = "new"
    ),
    processed_docs = c("one.docx", "two.docx"),
    errors = tibble::tibble(file = character(0), error = character(0), traceback = character(0)),
    catalog = list(),
    redline_columns = names(redlines),
    redlines_rows = redlines,
    existing_docs = list(),
    fork_reviewers = ids,
    signoff_reviewers = character(0),
    documents_sheet_mode = "columns"
  )
}

# Cells of one sheet of an .xlsx as a data frame: type, value (shared strings
# resolved), formula and the horizontal alignment of the cell's style
read_sheet_cells <- function(path, sheet_file = "sheet1.xml") {
  td <- tempfile("xl_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = td)
  ns <- c(m = "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
  strings <- character(0)
  if (file.exists(file.path(td, "xl", "sharedStrings.xml"))) {
    ss <- xml2::read_xml(file.path(td, "xl", "sharedStrings.xml"))
    strings <- vapply(xml2::xml_find_all(ss, "//m:si", ns), function(si) {
      paste(xml2::xml_text(xml2::xml_find_all(si, ".//m:t", ns)), collapse = "")
    }, character(1))
  }
  styles <- xml2::read_xml(file.path(td, "xl", "styles.xml"))
  xfs <- xml2::xml_find_all(styles, "//m:cellXfs/m:xf", ns)
  halign <- vapply(xfs, function(xf) {
    a <- xml2::xml_find_first(xf, "m:alignment", ns)
    if (is.na(xml2::xml_name(a))) NA_character_ else xml2::xml_attr(a, "horizontal")
  }, character(1))
  sheet <- xml2::read_xml(file.path(td, "xl", "worksheets", sheet_file))
  cells <- xml2::xml_find_all(sheet, "//m:sheetData/m:row/m:c", ns)
  type <- xml2::xml_attr(cells, "t")
  value <- xml2::xml_text(xml2::xml_find_first(cells, "m:v", ns))
  s <- xml2::xml_attr(cells, "s")
  data.frame(
    ref = xml2::xml_attr(cells, "r"),
    type = ifelse(is.na(type), "", type),
    value = ifelse(!is.na(type) & type == "s", strings[as.integer(value) + 1L], value),
    formula = xml2::xml_text(xml2::xml_find_first(cells, "m:f", ns)),
    halign = ifelse(is.na(s), NA_character_, halign[as.integer(s) + 1L]),
    stringsAsFactors = FALSE
  )
}

test_that("a workbook is written with a number of openxlsx calls that does not grow with the rows (A6-09)", {
  skip_if_not_installed("openxlsx")
  calls <- new.env()
  calls$n <- 0L
  count <- function(name) {
    force(name)
    function(...) {
      calls$n <- calls$n + 1L
      invisible(NULL)
    }
  }
  # the writes and styles are the cost: count them (without doing them, so the
  # measurement is quick however the code under test is written)
  local_mocked_bindings(
    writeData = count("writeData"), writeFormula = count("writeFormula"), addStyle = count("addStyle"),
    .package = "openxlsx"
  )
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)

  calls$n <- 0L
  do.call(write_review_tracker_excel, c(writer_args(6, 4), list(output_path = out)))
  small <- calls$n
  calls$n <- 0L
  do.call(write_review_tracker_excel, c(writer_args(120, 80), list(output_path = out)))
  large <- calls$n

  expect_gt(small, 0L)
  # one write per column and one style per block: 20 times the rows, the same calls
  expect_equal(large, small)
  # and a handful of calls per column of each sheet, not one per cell
  expect_lt(large, 400L)
})

test_that("the vectorised writer keeps every kind of cell, formula and style (A6-09)", {
  skip_if_not_installed("openxlsx")
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  args <- writer_args(6, 4)
  args$comments_rows$comment_text[2] <- NA
  args$comments_rows$paragraph_number[3] <- NA
  args$comments_rows$comment_text[4] <- "=HYPERLINK(\"x\")"
  do.call(write_review_tracker_excel, c(args, list(output_path = out)))

  cm <- read_sheet_cells(out, "sheet1.xml")
  cell <- function(ref) cm[cm$ref == ref, ]
  cols <- args$columns
  col_ref <- function(name, row) paste0(openxlsx::int2col(match(name, cols)), row)

  # header row: titles, one centered style
  expect_equal(cell(col_ref("comment_text", 1))$value, "comment_text")
  expect_equal(cell(col_ref("comment_text", 1))$halign, "center")
  # text: a missing value is an empty string cell, text that looks like a formula is a string
  expect_equal(cell(col_ref("comment_text", 3))$type, "s")
  expect_equal(cell(col_ref("comment_text", 3))$value, "")
  expect_equal(cell(col_ref("comment_text", 5))$value, "=HYPERLINK(\"x\")")
  expect_equal(cell(col_ref("comment_text", 5))$formula, NA_character_)
  expect_equal(cell(col_ref("comment_text", 2))$halign, "left")
  # counts: a number, and an empty string (not a blank cell) where it is missing
  expect_equal(cell(col_ref("paragraph_number", 2))$type, "n")
  expect_equal(cell(col_ref("paragraph_number", 2))$value, "1")
  expect_equal(cell(col_ref("paragraph_number", 4))$type, "s")
  expect_equal(cell(col_ref("paragraph_number", 4))$value, "")
  expect_equal(cell(col_ref("paragraph_number", 2))$halign, "center")
  # flags: TRUE / FALSE are booleans, a missing value is a blank cell
  expect_equal(cell(col_ref("rev_a", 2))$type, "b")
  expect_equal(cell(col_ref("rev_a", 2))$value, "1")
  expect_equal(cell(col_ref("rev_a", 3))$type, "")
  expect_equal(cell(col_ref("rev_b", 2))$value, "0")
  # resolved: one live formula per row, pointing at that row's reviewer cells
  res2 <- cell(col_ref("resolved", 2))$formula
  expect_match(res2, paste0("^=IF\\(AND\\(OR\\(", col_ref("rev_a", 2), "=TRUE,"))
  expect_match(cell(col_ref("resolved", 7))$formula, paste0(col_ref("rev_b", 7), '="TRUE"'))
  expect_equal(cell(col_ref("resolved", 2))$halign, "center")

  # the same values from text-typed rows (as read back from an existing tracker)
  args_text <- writer_args(6, 4, typed = FALSE)
  out_text <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_text), add = TRUE)
  do.call(write_review_tracker_excel, c(args_text, list(output_path = out_text)))
  cm_text <- read_sheet_cells(out_text, "sheet1.xml")
  expect_equal(cm_text$formula, cm$formula)
  expect_equal(cm_text$halign, cm$halign)
})

test_that("the Documents rollup reaches the last row written, however many rows there are (A6-21)", {
  skip_if_not_installed("openxlsx")
  n_late <- 5200L
  files <- c(rep("early.docx", 3), rep("late.docx", n_late))
  comments <- tibble::tibble(file = files, rev_a = TRUE)   # everything signed off
  empty_rl <- tibble::tibble(file = character(0), rev_a = logical(0))
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  write_review_tracker_excel(
    columns = c("file", "rev_a"), comments_rows = comments,
    revisions_rows = tibble::tibble(
      file = character(0), revision_type = character(0), author = character(0), date = character(0),
      changed_text = character(0), paragraph_number = integer(0), paragraph_context = character(0)
    ),
    processed_docs = c("early.docx", "late.docx"),
    errors = tibble::tibble(file = character(0), error = character(0), traceback = character(0)),
    output_path = out, catalog = list(), redline_columns = c("file", "rev_a"), redlines_rows = empty_rl,
    existing_docs = list(), fork_reviewers = "rev_a", signoff_reviewers = character(0),
    documents_sheet_mode = "columns"
  )

  # Documents is the fourth sheet created (sheet4.xml): file, pipeline_stage, four
  # counts, then the rev_a rollup in column G and resolved in column H
  cells <- read_sheet_cells(out, "sheet4.xml")
  expect_equal(cells$value[cells$ref == "G1"], "rev_a")
  late <- cells$formula[cells$ref == "G3"]
  bound <- as.integer(sub(".*Comments!\\$A\\$2:\\$A\\$(\\d+)=A3.*", "\\1", late))
  last_row <- nrow(comments) + 1L
  expect_gte(bound, last_row)
  expect_gt(bound, last_row)   # with headroom

  # what the two terms of the formula evaluate to, for every document
  rows <- seq_len(nrow(comments)) + 1L
  for (doc in c("early.docx", "late.docx")) {
    sumproduct <- sum(rows <= bound & files == doc & comments$rev_a)
    countif <- sum(files == doc)
    expect_equal(sumproduct, countif, info = doc)
  }
  # the resolved column (H) uses the same bound
  expect_match(cells$formula[cells$ref == "H3"], sprintf("Comments!\\$A\\$2:\\$A\\$%d=A3", bound))

  # small trackers keep the formula text they always had
  expect_equal(rollup_bound(0), 5000L)
  expect_equal(rollup_bound(3999), 5000L)
  expect_equal(rollup_bound(10000), 11001L)
})

test_that("the master and the forks of one run are written from data prepared once (A6-09)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()
  body <- '<w:p><w:commentRangeStart w:id="1"/><w:r><w:t>hello</w:t></w:r><w:commentRangeEnd w:id="1"/></w:p>'
  write_test_docx(
    file.path(d, "one.docx"), body,
    comments_part('<w:comment w:id="1" w:author="Ann" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>note</w:t></w:r></w:p></w:comment>')
  )
  n_prepared <- 0L
  real <- prepare_review_workbook_data
  local_mocked_bindings(prepare_review_workbook_data = function(...) {
    n_prepared <<- n_prepared + 1L
    real(...)
  })
  res <- cbe_docx_review_extract(
    d, output_dir = file.path(d, "out"),
    fork_reviewers = c("rev_a", "rev_b", "rev_c"), signoff_reviewers = "lead"
  )
  expect_equal(n_prepared, 1L)
  expect_length(res$paths$forks, 3L)
  expect_true(all(file.exists(res$paths$forks)))
})

test_that("merging does not clean every existing row once per incoming row (A6-09)", {
  catalog <- list()
  n <- 60
  existing <- tibble::tibble(
    file = "one.docx", comment_id = as.character(seq_len(n)), author = "Ann", date = "2026-01-01T10:00:00Z",
    comment_text = paste("existing comment", seq_len(n)), selected_text = "", paragraph_number = as.character(seq_len(n)),
    end_paragraph_number = NA_character_, paragraph_context = "", rev_a = NA_character_, doc_status = "Active"
  )
  incoming <- tibble::tibble(
    file = "one.docx", comment_id = as.character(seq_len(n)), author = "Ann", date = "2026-01-01T10:00:00Z",
    comment_text = paste("existing comment", seq_len(n)), selected_text = "", paragraph_number = seq_len(n),
    end_paragraph_number = NA_integer_, paragraph_context = "", is_reply = FALSE, reply_to_id = NA_character_
  )
  cleaned <- 0L
  real <- clean_review_text
  local_mocked_bindings(clean_review_text = function(text) {
    cleaned <<- cleaned + length(text)
    real(text)
  })
  merged <- merge_comments(incoming, existing, names(existing), catalog, "one.docx",
                           fork_reviewers = "rev_a", signoff_reviewers = character(0))
  expect_equal(nrow(merged$rows), n)
  # linear in the number of rows (a few passes over each table), not n * n = 3,600
  expect_lt(cleaned, 20 * n)

  cleaned <- 0L
  existing_rl <- tibble::tibble(
    file = "one.docx", paragraph_number = as.character(seq_len(n)), author = "Ann", date = NA_character_,
    original_text = paste("old", seq_len(n)), accepted_text = paste("new", seq_len(n)), is_toc_or_lof = "FALSE",
    rev_a = NA_character_, doc_status = "Active"
  )
  incoming_rl <- tibble::tibble(
    file = "one.docx", paragraph_number = seq_len(n), author = "Ann", date = NA_character_,
    original_text = paste("old", seq_len(n)), accepted_text = paste("new", seq_len(n)), is_toc_or_lof = FALSE
  )
  merged_rl <- merge_redlines(incoming_rl, existing_rl, names(existing_rl), catalog, "one.docx",
                              fork_reviewers = "rev_a", signoff_reviewers = character(0))
  expect_equal(nrow(merged_rl$rows), n)
  expect_lt(cleaned, 20 * n)
})

test_that("changes and comment ranges in footnotes, endnotes, headers and footers are warned about, not dropped silently (A6-19)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()
  body <- "<w:p><w:r><w:t>body text</w:t></w:r></w:p>"
  footnotes <- paste0(
    XML_PROLOG, "<w:footnotes ", W_DECL, '><w:footnote w:id="1"><w:p>',
    '<w:commentRangeStart w:id="9"/>',
    '<w:ins w:id="40" w:author="Dan" w:date="2026-01-04T10:00:00Z"><w:r><w:t>footnote insertion text</w:t></w:r></w:ins>',
    '<w:commentRangeEnd w:id="9"/></w:p></w:footnote></w:footnotes>'
  )
  header <- paste0(
    XML_PROLOG, "<w:hdr ", W_DECL, "><w:p>",
    '<w:del w:id="41" w:author="Eve" w:date="2026-01-05T10:00:00Z"><w:r><w:delText>old header text</w:delText></w:r></w:del>',
    "</w:p></w:hdr>"
  )
  # an endnotes part with only a separator, and a paragraph-mark marker, holds nothing to report
  endnotes <- paste0(
    XML_PROLOG, "<w:endnotes ", W_DECL, '><w:endnote w:id="0"><w:p><w:pPr><w:rPr>',
    '<w:ins w:id="42" w:author="Eve" w:date="2026-01-05T10:00:00Z"/></w:rPr></w:pPr></w:p></w:endnote></w:endnotes>'
  )
  cmt <- comments_part(
    '<w:comment w:id="9" w:author="Dan" w:date="2026-01-04T10:00:00Z"><w:p><w:r><w:t>about the footnote</w:t></w:r></w:p></w:comment>'
  )
  write_test_docx(
    file.path(d, "notes.docx"), body, cmt,
    parts = list("word/footnotes.xml" = footnotes, "word/header1.xml" = header, "word/endnotes.xml" = endnotes)
  )
  plain_footnotes <- paste0(
    XML_PROLOG, "<w:footnotes ", W_DECL, '><w:footnote w:id="1"><w:p><w:r><w:t>plain footnote</w:t></w:r></w:p></w:footnote></w:footnotes>'
  )
  write_test_docx(file.path(d, "clean.docx"), body, NULL, parts = list("word/footnotes.xml" = plain_footnotes))

  res <- extract_from_docx(file.path(d, "notes.docx"))
  un <- res$unread_parts
  expect_equal(sort(un$part), c("word/footnotes.xml", "word/header1.xml"))
  expect_equal(un$tracked_changes[un$part == "word/footnotes.xml"], 1L)
  expect_equal(un$comment_ranges[un$part == "word/footnotes.xml"], 1L)
  expect_equal(un$tracked_changes[un$part == "word/header1.xml"], 1L)
  expect_equal(un$comment_ranges[un$part == "word/header1.xml"], 0L)
  # the limitation, as documented: not extracted, and the comment anchored there has no location
  expect_equal(nrow(res$revisions), 0L)
  expect_true(is.na(res$comments$paragraph_number[res$comments$comment_id == "9"]))
  expect_equal(res$comments$selected_text[res$comments$comment_id == "9"], "")
  expect_equal(nrow(extract_from_docx(file.path(d, "clean.docx"))$unread_parts), 0L)

  warned <- capture_warnings(
    out <- cbe_docx_review_extract(
      d, output_dir = file.path(d, "out"),
      fork_reviewers = "rev_a", signoff_reviewers = character(0)
    )
  )
  expect_length(warned, 1L)
  expect_match(warned, "notes\\.docx \\(footnotes\\.xml: 1 tracked change\\(s\\) and 1 comment range\\(s\\); header1\\.xml: 1 tracked change\\(s\\)\\)")
  expect_false(grepl("clean.docx", warned, fixed = TRUE))
  expect_false(grepl("endnotes.xml", warned, fixed = TRUE))
  # the run itself is unaffected
  expect_equal(sort(out$documents$file), c("clean.docx", "notes.docx"))

  # nothing to warn about: no warning
  unlink(file.path(d, "notes.docx"))
  expect_no_warning(cbe_docx_review_extract(
    d, output_dir = file.path(d, "out2"),
    fork_reviewers = "rev_a", signoff_reviewers = character(0)
  ))
})

test_that("an archive over a limit is reported in $errors before anything is extracted (A6-22)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()
  body <- paste0(
    '<w:p><w:commentRangeStart w:id="1"/><w:r><w:t>hello</w:t></w:r><w:commentRangeEnd w:id="1"/></w:p>'
  )
  cmt <- comments_part(
    '<w:comment w:id="1" w:author="Ann" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>note</w:t></w:r></w:p></w:comment>'
  )
  write_test_docx(file.path(d, "fine.docx"), body, cmt)
  write_test_docx(
    file.path(d, "many.docx"), body, cmt,
    parts = stats::setNames(rep(list("x"), 12), sprintf("word/media/f%02d.bin", 1:12))
  )
  # 3 MB of zeros: a few KB compressed
  write_test_docx(file.path(d, "bulky.docx"), body, cmt, parts = list("word/media/big.bin" = strrep("0", 3e6)))

  # too many entries
  expect_error(
    extract_from_docx(file.path(d, "many.docx"), docx_limits = list(max_entries = 5)),
    "entries.*max_entries"
  )
  # too much once uncompressed
  expect_error(
    extract_from_docx(file.path(d, "bulky.docx"), docx_limits = list(max_total_bytes = 1e6)),
    "uncompressed.*max_total_bytes"
  )
  # an XML part that is read is too large
  expect_error(
    extract_from_docx(file.path(d, "fine.docx"), docx_limits = list(max_xml_bytes = 200)),
    "word/document.xml.*max_xml_bytes"
  )

  # the whole run reports the file and carries on with the others
  expect_warning(
    res <- cbe_docx_review_extract(
      d, output_dir = file.path(d, "out"),
      fork_reviewers = "rev_a", signoff_reviewers = character(0),
      docx_limits = list(max_entries = 6, max_total_bytes = 1e6)
    ),
    "2 Word file\\(s\\) could not be read"
  )
  expect_setequal(res$errors$file, c("many.docx", "bulky.docx"))
  expect_match(res$errors$error[res$errors$file == "many.docx"], "entries")
  expect_match(res$errors$error[res$errors$file == "bulky.docx"], "uncompressed")
  expect_equal(res$comments$file, "fine.docx")

  # generous by default: the same archives are read
  expect_equal(nrow(extract_from_docx(file.path(d, "many.docx"))$comments), 1L)
  expect_equal(nrow(extract_from_docx(file.path(d, "bulky.docx"))$comments), 1L)
})

test_that("only the parts that are read are extracted from the archive (A6-22)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  body <- "<w:p><w:r><w:t>hello</w:t></w:r></w:p>"
  write_test_docx(
    file.path(d, "media.docx"), body,
    comments_part('<w:comment w:id="1" w:author="Ann" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>note</w:t></w:r></w:p></w:comment>'),
    parts = list("word/media/big.bin" = strrep("0", 2e6), "word/styles.xml" = "<styles/>")
  )
  asked <- NULL
  local_mocked_bindings(unzip_docx_parts = function(docx_path, parts, exdir) {
    asked <<- parts
    utils::unzip(docx_path, files = parts, exdir = exdir)
  })
  res <- extract_from_docx(file.path(d, "media.docx"))
  expect_setequal(asked, c("word/document.xml", "word/comments.xml"))
  expect_equal(res$comments$comment_text, "note")
})

test_that("docx_limits is validated and its defaults are generous (A6-22)", {
  lim <- resolve_docx_limits()
  expect_equal(lim$max_entries, 10000)
  expect_equal(lim$max_total_bytes, 2 * 1024^3)
  expect_equal(lim$max_xml_bytes, 100 * 1024^2)
  expect_equal(resolve_docx_limits(list(max_entries = 3))$max_entries, 3)
  expect_equal(resolve_docx_limits(list(max_xml_bytes = Inf))$max_xml_bytes, Inf)

  withr::local_options(review.docx_max_entries = 7)
  expect_equal(resolve_docx_limits()$max_entries, 7)

  expect_error(resolve_docx_limits(list(max_entries = -1)), "positive number")
  expect_error(resolve_docx_limits(list(max_entries = "many")), "positive number")
  expect_error(resolve_docx_limits(list(max_files = 3)), "Unknown")
  expect_error(resolve_docx_limits(5), "named list")
  expect_error(cbe_docx_review_extract(tempdir(), docx_limits = list(max_entries = 0)), "positive number")
})

# A drawing Word stores twice: mc:Choice for current readers, mc:Fallback (VML) for older ones
text_box_xml <- function(inner) {
  paste0(
    "<w:pict><v:textbox><w:txbxContent><w:p>", inner, "</w:p></w:txbxContent></v:textbox></w:pict>"
  )
}
ins_run <- function(id, author, date, text) {
  sprintf('<w:ins w:id="%d" w:author="%s" w:date="%s"><w:r><w:t>%s</w:t></w:r></w:ins>', id, author, date, text)
}
del_run <- function(id, author, date, text) {
  sprintf('<w:del w:id="%d" w:author="%s" w:date="%s"><w:r><w:delText>%s</w:delText></w:r></w:del>', id, author, date, text)
}

write_text_box_docx <- function(path) {
  box <- text_box_xml(paste0(
    '<w:commentRangeStart w:id="6"/>', ins_run(30, "Cat", "2026-01-03T10:00:00Z", "boxed text"), '<w:commentRangeEnd w:id="6"/>'
  ))
  body <- paste0(
    "<w:p><w:r><w:t>P1 intro paragraph</w:t></w:r></w:p>",
    "<w:p><w:r><w:t>P2 keep </w:t></w:r>",
    ins_run(11, "Ann", "2026-01-01T10:00:00Z", "inserted words"),
    del_run(12, "Bob", "2026-01-02T10:00:00Z", "deleted words"), "</w:p>",
    "<w:p><w:r><mc:AlternateContent><mc:Choice Requires=\"wps\">", box, "</mc:Choice>",
    "<mc:Fallback>", box, "</mc:Fallback></mc:AlternateContent></w:r></w:p>",
    '<w:p><w:commentRangeStart w:id="5"/><w:r><w:t>P4 after the box</w:t></w:r><w:commentRangeEnd w:id="5"/></w:p>',
    "<w:p>", ins_run(13, "Ann", "2026-01-04T10:00:00Z", "first by Ann "),
    del_run(14, "Bob", "2026-01-05T10:00:00Z", "gone by Bob "),
    ins_run(15, "Ann", "2026-01-06T10:00:00Z", "second by Ann"), "</w:p>",
    "<w:p>", ins_run(16, "Dan", "2026-01-07T10:00:00Z", "only Dan edited this"), "</w:p>"
  )
  comments <- comments_part(
    '<w:comment w:id="5" w:author="Ann" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>after the box</w:t></w:r></w:p></w:comment>',
    '<w:comment w:id="6" w:author="Bob" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>in the box</w:t></w:r></w:p></w:comment>'
  )
  write_test_docx(path, body, comments, document_ns = MC_DECL)
}

test_that("a text box is read once, as part of the paragraph that holds it, and paragraphs are numbered as Word numbers them (A6-17)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  write_text_box_docx(file.path(d, "box.docx"))
  res <- extract_from_docx(file.path(d, "box.docx"))

  # one insertion for the box (the Fallback copy is not read), on the host paragraph 3
  boxed <- res$revisions[res$revisions$changed_text == "boxed text", ]
  expect_equal(nrow(boxed), 1L)
  expect_equal(boxed$paragraph_number, 3L)
  expect_equal(boxed$paragraph_context, "boxed text")
  expect_equal(
    res$revisions$paragraph_number[res$revisions$changed_text == "inserted words"], 2L
  )

  # no redline rows for the text box's own paragraphs: 2 (Ann + Bob), 3 (the box),
  # then the two paragraphs after the box, which Word numbers 5 and 6
  expect_equal(sort(res$redlines$paragraph_number), c(2L, 3L, 5L, 6L))
  box_row <- res$redlines[res$redlines$paragraph_number == 3L, ]
  expect_equal(box_row$original_text, "")
  expect_equal(box_row$accepted_text, "boxed text")
  expect_equal(box_row$author, "Cat")

  # a comment after the box is paragraph 4; one inside the box belongs to the host paragraph
  cm <- res$comments
  expect_equal(cm$paragraph_number[cm$comment_id == "5"], 4L)
  expect_equal(cm$selected_text[cm$comment_id == "5"], "P4 after the box")
  expect_equal(cm$paragraph_number[cm$comment_id == "6"], 3L)
  expect_equal(cm$selected_text[cm$comment_id == "6"], "boxed text")
})

test_that("a redlined paragraph names every author who changed it (A6-18)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  write_text_box_docx(file.path(d, "box.docx"))
  rl <- extract_from_docx(file.path(d, "box.docx"))$redlines
  row <- function(n) rl[rl$paragraph_number == n, ]

  # Ann inserted and Bob deleted: the old row named only Bob, the last editor
  expect_equal(row(2L)$author, "Ann; Bob")
  expect_equal(row(2L)$date, "2026-01-01T10:00:00Z; 2026-01-02T10:00:00Z")
  # authors in the order of their first change, each once; the date is that author's last change
  expect_equal(row(5L)$author, "Ann; Bob")
  expect_equal(row(5L)$date, "2026-01-06T10:00:00Z; 2026-01-05T10:00:00Z")
  # one author: as before
  expect_equal(row(6L)$author, "Dan")
  expect_equal(row(6L)$date, "2026-01-07T10:00:00Z")
  expect_equal(row(3L)$author, "Cat")

  # and it reaches the tracker
  skip_if_not_installed("openxlsx")
  res <- cbe_docx_review_extract(
    d, output_dir = file.path(d, "out"),
    fork_reviewers = "rev_a", signoff_reviewers = character(0)
  )
  expect_equal(res$redlines$author[res$redlines$paragraph_number == 2L], "Ann; Bob")
  sheet <- openxlsx::read.xlsx(res$paths$master, sheet = "SuggestedChanges")
  expect_equal(sheet$author[sheet$paragraph_number == 2L], "Ann; Bob")
})

test_that("a stem that ends in lowercase letters is matched whole before any suffix is stripped (A6-15)", {
  # invented catalog: two stages whose names legitimately end in 3 lowercase letters
  catalog <- list(
    alpha_summary     = list(rank = 0L, heading = 1, name = "Alpha summary", stage = "Stage A"),
    alpha_summary_uni = list(rank = 1L, heading = 1, name = "Alpha summary (uni)", stage = "Stage A uni"),
    alpha_summary_pca = list(rank = 2L, heading = 1, name = "Alpha summary (pca)", stage = "Stage A pca")
  )
  m <- function(f) match_docx_to_pipeline(f, catalog)

  expect_equal(m("alpha_summary.docx")$stage, "Stage A")
  expect_equal(m("alpha_summary_uni.docx")$matched_stem, "alpha_summary_uni")
  expect_equal(m("alpha_summary_uni.docx")$stage, "Stage A uni")
  expect_equal(m("alpha_summary_pca.docx")$stage, "Stage A pca")
  expect_equal(m("alpha_summary_pca.docx")$rank, 2L)
  # the date is still stripped first
  expect_equal(m("alpha_summary_uni_11_21_25.docx")$matched_stem, "alpha_summary_uni")
  # reviewer initials are still stripped when the name without them is what the catalog holds
  expect_equal(m("alpha_summary_ABC_11_21_25.docx")$matched_stem, "alpha_summary")
  expect_equal(m("alpha_summary_xy.docx")$matched_stem, "alpha_summary")
  expect_equal(m("alpha_summary_uni_xy_11_21_25.docx")$matched_stem, "alpha_summary_uni")

  # the stem helper itself: whole when it is a known stem, stripped otherwise
  expect_equal(extract_docx_stem("alpha_summary_uni.docx", known_stems = names(catalog)), "alpha_summary_uni")
  expect_equal(extract_docx_stem("alpha_summary_uni.docx"), "alpha_summary")
})

test_that("a file that can no longer be read keeps the rows the tracker already holds (A6-11 with A6-08)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()
  body <- '<w:p><w:commentRangeStart w:id="1"/><w:r><w:t>hello</w:t></w:r><w:commentRangeEnd w:id="1"/></w:p>'
  good <- comments_part('<w:comment w:id="1" w:author="Ann" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>first round note</w:t></w:r></w:p></w:comment>')
  write_test_docx(file.path(d, "eda.docx"), body, good)
  args <- list(d, output_dir = file.path(d, "out"), fork_reviewers = "rev_a", signoff_reviewers = character(0))
  first <- do.call(cbe_docx_review_extract, args)
  expect_equal(first$comments$comment_text, "first round note")

  # round two: the same file, now with a comments.xml that cannot be parsed
  write_test_docx(
    file.path(d, "eda.docx"), body,
    paste0(XML_PROLOG, "<w:comments ", W_DECL, '><w:comment w:id="1" w:author="Ann"><w:p><w:r><w:t>never closed')
  )
  expect_warning(second <- do.call(cbe_docx_review_extract, args), "could not be read")
  expect_equal(second$errors$file, "eda.docx")
  expect_equal(second$comments$comment_text, "first round note")
})

# word/commentsExtended.xml as Word lays it out: one w15:commentEx per comment,
# keyed by the w14:paraId of the comment's LAST paragraph
comments_extended_part <- function(...) {
  paste0(
    XML_PROLOG,
    '<w15:commentsEx xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml">',
    paste0(..., collapse = ""), "</w15:commentsEx>"
  )
}

# Two threads written the way Word writes them: every comment a sibling in
# comments.xml, replies linked and resolved only through commentsExtended.xml
write_threaded_docx <- function(path, extended = NULL) {
  anchor <- function(ids, text) {
    paste0(
      "<w:p>", paste0('<w:commentRangeStart w:id="', ids, '"/>', collapse = ""),
      "<w:r><w:t>", text, "</w:t></w:r>",
      paste0('<w:commentRangeEnd w:id="', ids, '"/><w:r><w:commentReference w:id="', ids, '"/></w:r>', collapse = ""),
      "</w:p>"
    )
  }
  body <- paste0(anchor(c(0, 1), "first target"), anchor(c(2, 3), "second target"), anchor(4, "third target"))
  cmt <- function(id, author, paras) {
    ps <- vapply(seq_along(paras), function(i) {
      sprintf('<w:p w14:paraId="%s"><w:r><w:t>%s</w:t></w:r></w:p>', names(paras)[i], paras[[i]])
    }, character(1))
    sprintf('<w:comment w:id="%d" w:author="%s" w:date="2026-01-01T10:00:00Z" w:initials="X">%s</w:comment>',
            id, author, paste(ps, collapse = ""))
  }
  comments <- comments_part(
    cmt(0, "Ann", c("0A000001" = "parent one")),
    cmt(1, "Bob", c("0B000001" = "reply to one")),
    cmt(2, "Ann", c("0C000001" = "first paragraph", "0C000002" = "last paragraph")),
    cmt(3, "Bob", c("0D000001" = "reply to two")),
    cmt(4, "Cat", c("0E000001" = "no extended entry"))
  )
  if (is.null(extended)) {
    extended <- comments_extended_part(
      '<w15:commentEx w15:paraId="0A000001" w15:done="0"/>',
      '<w15:commentEx w15:paraId="0B000001" w15:paraIdParent="0A000001" w15:done="0"/>',
      '<w15:commentEx w15:paraId="0C000002" w15:done="1"/>',
      '<w15:commentEx w15:paraId="0D000001" w15:paraIdParent="0C000002" w15:done="0"/>'
    )
  }
  write_test_docx(
    path, body, comments,
    parts = list("word/commentsExtended.xml" = extended),
    document_ns = ""
  )
}

test_that("replies and resolved threads are read from commentsExtended.xml, as Word lays them out (A6-07)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  write_threaded_docx(file.path(d, "threads.docx"))

  res <- extract_from_docx(file.path(d, "threads.docx"))
  cm <- res$comments
  row <- function(id) cm[cm$comment_id == id, ]

  expect_equal(nrow(cm), 5L)
  # thread one: an open parent and its reply
  expect_false(row("0")$is_reply)
  expect_true(row("1")$is_reply)
  expect_equal(row("1")$reply_to_id, "0")
  expect_false(row("0")$resolved_in_docx)
  expect_false(row("1")$resolved_in_docx)
  # thread two: the parent is resolved, and the link is keyed on the LAST paragraph
  # of a two-paragraph comment; the reply carries done = 0 but belongs to a resolved thread
  expect_true(row("2")$resolved_in_docx)
  expect_true(row("3")$is_reply)
  expect_equal(row("3")$reply_to_id, "2")
  expect_true(row("3")$resolved_in_docx)
  # a comment with no entry in commentsExtended.xml is an unresolved top-level comment
  expect_false(row("4")$is_reply)
  expect_true(is.na(row("4")$reply_to_id))
  expect_false(row("4")$resolved_in_docx)
})

test_that("thread and resolved state reach the merged table, the workbook and the document summary (A6-07)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()
  write_threaded_docx(file.path(d, "threads.docx"))

  res <- cbe_docx_review_extract(
    d, output_dir = file.path(d, "out"),
    fork_reviewers = "rev_a", signoff_reviewers = character(0)
  )
  cm <- res$comments
  expect_true("resolved_in_docx" %in% names(cm))
  expect_equal(cm$is_reply[match(c("0", "1", "2", "3", "4"), cm$comment_id)], c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(cm$resolved_in_docx[match(c("0", "1", "2", "3", "4"), cm$comment_id)], c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(cm$reply_to_id[match("3", cm$comment_id)], "2")

  # the Documents sheet counted no replies while is_reply never came out TRUE
  expect_equal(res$documents$reply_count, 2L)

  sheet <- openxlsx::read.xlsx(res$paths$master, sheet = "Comments")
  expect_true("resolved_in_docx" %in% names(sheet))
  expect_equal(
    as.character(sheet$resolved_in_docx[match(c("0", "2", "3"), sheet$comment_id)]),
    c("FALSE", "TRUE", "TRUE")
  )
  csv <- utils::read.csv(res$paths$comments_csv, check.names = FALSE)
  expect_true("resolved_in_docx" %in% names(csv))
})

test_that("a reply whose parent is not in comments.xml is still a reply, and the nested layout still works (A6-07)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  body <- paste0(
    '<w:p><w:commentRangeStart w:id="7"/><w:r><w:t>target</w:t></w:r>',
    '<w:commentRangeEnd w:id="7"/></w:p>'
  )
  comments <- comments_part(
    '<w:comment w:id="7" w:author="Bob" w:date="2026-01-01T10:00:00Z"><w:p w14:paraId="00000007"><w:r><w:t>orphan reply</w:t></w:r></w:p></w:comment>'
  )
  extended <- comments_extended_part('<w15:commentEx w15:paraId="00000007" w15:paraIdParent="DEADBEEF" w15:done="0"/>')
  write_test_docx(file.path(d, "orphan.docx"), body, comments, parts = list("word/commentsExtended.xml" = extended))
  cm <- extract_from_docx(file.path(d, "orphan.docx"))$comments
  expect_true(cm$is_reply)
  expect_true(is.na(cm$reply_to_id))

  # nested form (a reply written inside its parent, done as an attribute): unchanged
  nested <- comments_part(
    '<w:comment w:id="7" w:author="Ann" w:date="2026-01-01T10:00:00Z" w:done="0"><w:p><w:r><w:t>parent</w:t></w:r></w:p>',
    '<w:comment w:id="8" w:author="Bob" w:date="2026-01-01T11:00:00Z" w:done="1"><w:p><w:r><w:t>child</w:t></w:r></w:p></w:comment></w:comment>'
  )
  write_test_docx(file.path(d, "nested.docx"), body, nested)
  cm <- extract_from_docx(file.path(d, "nested.docx"))$comments
  expect_equal(cm$is_reply[cm$comment_id == "8"], TRUE)
  expect_equal(cm$reply_to_id[cm$comment_id == "8"], "7")
  expect_equal(cm$resolved_in_docx[cm$comment_id %in% c("7", "8")], c(FALSE, TRUE))
})

test_that("a commentsExtended.xml that cannot be parsed is reported like a broken comments.xml (A6-07)", {
  skip_if_not_installed("zip")
  d <- fid_tmp_dir()
  write_threaded_docx(
    file.path(d, "bad_ext.docx"),
    extended = paste0(XML_PROLOG, '<w15:commentsEx xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"><oops')
  )
  expect_error(
    extract_from_docx(file.path(d, "bad_ext.docx")),
    "word/commentsExtended.xml could not be parsed"
  )
})

test_that("a comments.xml that cannot be parsed is reported, not read as blank comments (A6-11)", {
  skip_if_not_installed("zip")
  skip_if_not_installed("openxlsx")
  d <- fid_tmp_dir()

  body <- paste0(
    '<w:p><w:commentRangeStart w:id="1"/><w:r><w:t>target</w:t></w:r>',
    '<w:commentRangeEnd w:id="1"/></w:p>'
  )
  broken_comments <- paste0(
    XML_PROLOG, "<w:comments ", W_DECL, '><w:comment w:id="1" w:author="Ann" ',
    'w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>never closed'
  )
  write_test_docx(file.path(d, "broken.docx"), body, broken_comments)
  write_test_docx(
    file.path(d, "fine.docx"), body,
    comments_part('<w:comment w:id="1" w:author="Bob" w:date="2026-01-01T10:00:00Z"><w:p><w:r><w:t>ok</w:t></w:r></w:p></w:comment>')
  )

  # The document's comment ranges are intact, so the old reader still produced a
  # row for the broken file, with no author and empty text.
  expect_error(
    extract_from_docx(file.path(d, "broken.docx")),
    "word/comments.xml could not be parsed"
  )

  expect_warning(
    res <- cbe_docx_review_extract(
      d, output_dir = file.path(d, "out"),
      fork_reviewers = "rev_a", signoff_reviewers = character(0)
    ),
    "1 Word file\\(s\\) could not be read"
  )
  expect_equal(res$errors$file, "broken.docx")
  expect_match(res$errors$error, "word/comments.xml could not be parsed")
  expect_false("broken.docx" %in% res$comments$file)
  expect_equal(res$comments$comment_text[res$comments$file == "fine.docx"], "ok")
})
