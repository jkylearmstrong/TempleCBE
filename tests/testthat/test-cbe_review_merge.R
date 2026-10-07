# Regression tests for the merge and identity correctness of the Word-review
# extractor (audit chunk A6): paragraph identity, typed merging, comment
# identity across rounds, fork and master edits, per-reviewer Documents sheets,
# unreadable documents, text fidelity and small robustness items.
#
# Every name below is invented (generic reviewer ids, generic file stems).

# =============================================================================
# HELPERS
# =============================================================================

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

# =============================================================================
# A6-01: PARAGRAPH IDENTITY
# =============================================================================

test_that("tracked changes, comments and redlines number paragraphs the same way (A6-01)", {
  d <- a6_dir()
  body <- paste0(
    vapply(1:10, function(i) {
      if (i == 2) a6_para("Second paragraph.", ins = "inserted two")
      else if (i == 3) a6_para("Third paragraph.", ins = "inserted three")
      else if (i == 6) sprintf(
        '<w:p><w:commentRangeStart w:id="7"/><w:r><w:t>Sixth paragraph.</w:t></w:r><w:commentRangeEnd w:id="7"/><w:ins w:id="9" w:author="Cat" w:date="2026-01-02T10:00:00Z"><w:r><w:t>inserted six</w:t></w:r></w:ins></w:p>'
      )
      else a6_para(sprintf("Paragraph %d.", i))
    }, ""),
    collapse = ""
  )
  docx <- file.path(d, "tenpara.docx")
  a6_docx(docx, body, comments = a6_comment(7, "check six"))

  res <- extract_from_docx(docx, min_revision_length = 2)

  rev <- res$revisions[order(res$revisions$paragraph_number), ]
  expect_equal(rev$changed_text, c("inserted two", "inserted three", "inserted six"))
  expect_equal(rev$paragraph_number, c(2L, 3L, 6L))
  expect_equal(
    rev$paragraph_context,
    c("Second paragraph.inserted two", "Third paragraph.inserted three", "Sixth paragraph.inserted six")
  )

  expect_equal(sort(res$redlines$paragraph_number), c(2L, 3L, 6L))
  expect_equal(res$comments$paragraph_number, 6L)

  # The same agreement holds in what a run reports (and sorts by)
  a6_options()
  out <- cbe_docx_review_extract(d, output_dir = file.path(d, "out"))
  expect_equal(out$revisions$paragraph_number, c(2L, 3L, 6L))
  tc <- a6_sheet(out$paths$master, "TrackedChanges")
  expect_equal(tc$paragraph_number, c("2", "3", "6"))
})

test_that("a tracked change inside a text box is numbered by the paragraph that holds it (A6-01)", {
  d <- a6_dir()
  nested <- paste0(
    '<w:p><w:r><w:t>Host.</w:t></w:r><w:r><w:pict><w:txbxContent>',
    '<w:p><w:ins w:id="3" w:author="Cat" w:date="2026-01-02T10:00:00Z"><w:r><w:t>boxed words</w:t></w:r></w:ins></w:p>',
    '</w:txbxContent></w:pict></w:r></w:p>'
  )
  docx <- file.path(d, "box.docx")
  a6_docx(docx, paste0(a6_para("First."), nested, a6_para("Last.")))
  res <- extract_from_docx(docx)
  # Word numbers First (1), Host (2), Last (3): the paragraph inside the text box
  # is part of Host, not a paragraph of its own (audit A6-17), so the change is in
  # paragraph 2 and its context is the host paragraph's text
  expect_equal(res$revisions$paragraph_number, 2L)
  expect_equal(res$revisions$paragraph_context, "Host.boxed words")
  expect_equal(res$redlines$paragraph_number, 2L)
})

# =============================================================================
# A6-03: NEW, MATCHED AND RETAINED ROWS IN ONE MERGE
# =============================================================================

test_that("merge_comments combines new, matched and retained rows of different storage types (A6-03)", {
  catalog <- load_pipeline_catalog(NULL)
  incoming <- tibble::tibble(
    file = "eda.docx", comment_id = c("0", "3"), author = "Ann",
    date = c("2026-01-01T10:00:00Z", "2026-01-05T10:00:00Z"),
    comment_text = c("kept comment", "brand new comment"),
    selected_text = "", paragraph_number = c(2L, 7L), end_paragraph_number = NA_integer_,
    paragraph_context = "", resolved_in_docx = FALSE, is_reply = c(FALSE, TRUE),
    reply_to_id = c(NA, "0")
  )
  # everything read back from a workbook is text
  existing <- tibble::tibble(
    file = c("eda.docx", "gone.docx"), comment_id = c("0", "5"), author = "Ann",
    date = c("2026-01-01T10:00:00Z", "2025-12-01T10:00:00Z"),
    comment_text = c("kept comment", "comment of a document no longer scanned"),
    paragraph_number = c("2", "4"), end_paragraph_number = c(NA, "6"),
    is_reply = c("FALSE", "TRUE"), rev_a = c(NA, "TRUE"), rev_a_comment = c("seen", NA),
    doc_status = c("Active", "Active"), duplicate_count = c("1", "1")
  )

  res <- merge_comments(
    incoming, existing, names(existing), catalog,
    scanned_files = "eda.docx", fork_reviewers = c("rev_a", "rev_b"), signoff_reviewers = character(0)
  )$rows

  expect_equal(nrow(res), 3)
  expect_setequal(res$comment_id, c("0", "3", "5"))
  expect_type(res$paragraph_number, "integer")
  expect_type(res$end_paragraph_number, "integer")
  expect_type(res$duplicate_count, "integer")
  expect_type(res$is_reply, "logical")
  expect_equal(res$paragraph_number[res$comment_id == "5"], 4L)
  expect_equal(res$end_paragraph_number[res$comment_id == "5"], 6L)
  expect_true(res$is_reply[res$comment_id == "5"])
  expect_equal(res$doc_status[res$comment_id == "5"], "Prior Round / Not in docx")
  expect_equal(res$rev_a_comment[res$comment_id == "0"], "seen")
  expect_equal(res$doc_status[res$comment_id == "3"], "Active")
})

test_that("merge_revisions and merge_redlines combine freshly extracted rows with rows read back as text (A6-03)", {
  catalog <- load_pipeline_catalog(NULL)

  incoming_rev <- tibble::tibble(
    file = "eda.docx", revision_type = "insertion", author = "Cat", date = "2026-01-02T10:00:00Z",
    changed_text = "new words", paragraph_number = 3L, paragraph_context = "ctx"
  )
  existing_rev <- tibble::tibble(
    file = c("eda.docx", "gone.docx"), revision_type = "deletion", author = "Bob",
    date = "2025-12-01T10:00:00Z", changed_text = c("old words", "gone words"),
    paragraph_number = c("2", "9"), paragraph_context = "ctx"
  )
  rev <- merge_revisions(incoming_rev, existing_rev, catalog, scanned_files = "eda.docx")
  expect_equal(nrow(rev), 2)  # the new row; the unscanned file's row is kept
  expect_setequal(rev$changed_text, c("new words", "gone words"))
  expect_type(rev$paragraph_number, "integer")
  expect_equal(rev$paragraph_number[rev$changed_text == "gone words"], 9L)

  incoming_rl <- tibble::tibble(
    file = "eda.docx", paragraph_number = c(1L, 5L), author = "Cat", date = "2026-01-02T10:00:00Z",
    original_text = c("kept paragraph", "other paragraph"),
    accepted_text = c("kept paragraph edited", "other paragraph edited"),
    is_toc_or_lof = c(FALSE, TRUE)
  )
  existing_rl <- tibble::tibble(
    file = c("eda.docx", "gone.docx"), paragraph_number = c("1", "8"), author = "Cat",
    date = "2025-12-01T10:00:00Z", original_text = c("kept paragraph", "vanished paragraph"),
    accepted_text = c("kept paragraph edited", "vanished paragraph edited"),
    is_toc_or_lof = c("FALSE", "TRUE"), rev_a = c("TRUE", "TRUE"), rev_a_comment = c("ok", "ok"),
    doc_status = "Active"
  )
  rl <- merge_redlines(
    incoming_rl, existing_rl, names(existing_rl), catalog, scanned_files = "eda.docx",
    fork_reviewers = c("rev_a", "rev_b"), signoff_reviewers = character(0)
  )$rows
  expect_equal(nrow(rl), 3)  # matched, new and retained
  expect_type(rl$paragraph_number, "integer")
  expect_type(rl$is_toc_or_lof, "logical")
  expect_equal(rl$rev_a_comment[rl$original_text == "kept paragraph"], "ok")
  expect_equal(rl$doc_status[rl$original_text == "vanished paragraph"], "Prior Round / Not in docx")
  expect_true(rl$is_toc_or_lof[rl$original_text == "vanished paragraph"])
})

test_that("a second round with a new comment, a signed-off comment and a document that left the folder merges (A6-03)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")

  a6_docx(
    file.path(d, "eda.docx"),
    paste0(a6_commented_para(0, "alpha text"), a6_commented_para(1, "beta text"), a6_para("Gamma.", ins = "added gamma")),
    comments = c(a6_comment(0, "first comment"), a6_comment(1, "second comment"))
  )
  a6_docx(
    file.path(d, "intro.docx"),
    paste0(a6_commented_para(0, "intro text"), a6_para("Intro.", ins = "added intro")),
    comments = a6_comment(0, "intro comment")
  )
  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(nrow(r1$comments), 3)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "second comment",
               list(rev_a = "TRUE", rev_a_comment = "checked"))

  # Round 2: intro.docx left the folder; eda.docx gained a comment (after the
  # others, so the old ones keep their ids) and a second tracked change
  file.remove(file.path(d, "intro.docx"))
  a6_docx(
    file.path(d, "eda.docx"),
    paste0(a6_commented_para(0, "alpha text"), a6_commented_para(1, "beta text"), a6_commented_para(2, "delta text"),
           a6_para("Gamma.", ins = "added gamma"), a6_para("Epsilon.", ins = "added epsilon")),
    comments = c(a6_comment(0, "first comment"), a6_comment(1, "second comment"), a6_comment(2, "third comment"))
  )
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)

  expect_setequal(r2$comments$comment_text, c("first comment", "second comment", "third comment", "intro comment"))
  expect_equal(r2$comments$rev_a[r2$comments$comment_text == "second comment"], "TRUE")
  expect_equal(r2$comments$doc_status[r2$comments$comment_text == "intro comment"], "Prior Round / Not in docx")
  expect_setequal(r2$revisions$changed_text, c("added gamma", "added epsilon", "added intro"))
  expect_setequal(r2$redlines$file, c("eda.docx", "intro.docx"))
  expect_equal(sum(r2$redlines$file == "eda.docx"), 2)

  expect_type(r2$comments$paragraph_number, "integer")
  expect_type(r2$comments$is_reply, "logical")
  expect_type(r2$revisions$paragraph_number, "integer")
  expect_type(r2$redlines$paragraph_number, "integer")
  expect_type(r2$redlines$is_toc_or_lof, "logical")

  # and the round-2 workbook can be merged again
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(nrow(r3$comments), 4)
})

test_that("a signed-off comment deleted from the document is kept as a prior-round row (A6-03)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  docx <- file.path(d, "eda.docx")

  a6_docx(docx, paste0(a6_commented_para(0, "alpha"), a6_commented_para(1, "beta")),
          comments = c(a6_comment(0, "keep me"), a6_comment(1, "delete me")))
  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "delete me", list(rev_a = "TRUE"))

  a6_docx(docx, a6_commented_para(0, "alpha"), comments = a6_comment(0, "keep me"))
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)

  expect_equal(nrow(r2$comments), 2)
  gone <- r2$comments[r2$comments$comment_text == "delete me", ]
  expect_equal(gone$rev_a, "TRUE")
  expect_equal(gone$doc_status, "Prior Round / Not in docx")
})

# =============================================================================
# A6-04: A COMMENT IS IDENTIFIED BY WHAT IT SAYS, NOT BY ITS WORD ID
# =============================================================================

# Freshly extracted comments (typed) and existing tracker rows (text)
a6_incoming <- function(ids, texts, author = "Ann", date = "2026-01-01T10:00:00Z", para = seq_along(ids)) {
  tibble::tibble(
    file = "eda.docx", comment_id = as.character(ids), author = author, date = date,
    comment_text = texts, selected_text = "", paragraph_number = as.integer(para),
    end_paragraph_number = NA_integer_, paragraph_context = "", resolved_in_docx = FALSE,
    is_reply = FALSE, reply_to_id = NA_character_
  )
}
a6_existing <- function(ids, texts, rev_a = NA, note = NA, author = "Ann", date = "2026-01-01T10:00:00Z",
                        para = seq_along(ids)) {
  tibble::tibble(
    file = "eda.docx", comment_id = as.character(ids), author = author, date = date,
    comment_text = texts, paragraph_number = as.character(para),
    rev_a = rev_a, rev_a_comment = note, doc_status = "Active"
  )
}
a6_merge <- function(incoming, existing) {
  merge_comments(
    incoming, existing, names(existing), load_pipeline_catalog(NULL),
    scanned_files = "eda.docx", fork_reviewers = c("rev_a", "rev_b"), signoff_reviewers = character(0)
  )$rows
}

test_that("a sign-off follows its comment when Word renumbers the comment ids (A6-04)", {
  existing <- a6_existing(0:1, c("first comment", "second comment"),
                          rev_a = c(NA, "TRUE"), note = c(NA, "I checked the SECOND comment"))
  # the author reordered the paragraphs: Word wrote 'second' as id 0 and 'first' as id 1
  res <- a6_merge(a6_incoming(0:1, c("second comment", "first comment")), existing)

  expect_equal(nrow(res), 2)
  second <- res[res$comment_text == "second comment", ]
  first <- res[res$comment_text == "first comment", ]
  expect_equal(second$comment_id, "0")
  expect_equal(second$rev_a, "TRUE")
  expect_equal(second$rev_a_comment, "I checked the SECOND comment")
  expect_equal(first$comment_id, "1")
  expect_true(is.na(first$rev_a))
  expect_true(is.na(first$rev_a_comment))
})

test_that("a changed comment text is a new comment and the signed-off row stays as a prior-round row (A6-04)", {
  existing <- a6_existing(0:1, c("first comment", "old wording"), rev_a = c(NA, "TRUE"), note = c(NA, "agreed"))
  res <- a6_merge(a6_incoming(0:1, c("first comment", "new wording")), existing)

  expect_equal(nrow(res), 3)
  new <- res[res$comment_text == "new wording", ]
  old <- res[res$comment_text == "old wording", ]
  expect_equal(new$doc_status, "Active")
  expect_true(is.na(new$rev_a))
  expect_equal(old$doc_status, "Prior Round / Not in docx")
  expect_equal(old$rev_a, "TRUE")
  expect_equal(old$rev_a_comment, "agreed")

  # a changed text with no sign-off has nothing to keep: only the new row remains
  plain <- a6_existing(0:1, c("first comment", "old wording"))
  res2 <- a6_merge(a6_incoming(0:1, c("first comment", "new wording")), plain)
  expect_equal(res2$comment_text, c("first comment", "new wording"))
})

test_that("identical comment texts are told apart by author, then by date (A6-04)", {
  # same text, two authors: the sign-off belongs to Ann's comment wherever it moves
  existing <- tibble::tibble(
    file = "eda.docx", comment_id = c("0", "1"), author = c("Ann", "Bob"),
    date = "2026-01-01T10:00:00Z", comment_text = "Please fix this.",
    paragraph_number = c("1", "2"), rev_a = c("TRUE", NA), rev_a_comment = c("Ann's was fixed", NA),
    doc_status = "Active"
  )
  incoming <- a6_incoming(0:1, "Please fix this.", author = c("Bob", "Ann"))
  res <- a6_merge(incoming, existing)
  expect_equal(res$rev_a[res$author == "Ann"], "TRUE")
  expect_true(is.na(res$rev_a[res$author == "Bob"]))

  # same text and author, two dates
  existing2 <- a6_existing(0:1, "Please fix this.", rev_a = c(NA, "TRUE"),
                           date = c("2026-01-01T10:00:00Z", "2026-01-03T10:00:00Z"))
  incoming2 <- a6_incoming(0:1, "Please fix this.", date = c("2026-01-03T10:00:00Z", "2026-01-01T10:00:00Z"))
  res2 <- a6_merge(incoming2, existing2)
  expect_equal(res2$rev_a[res2$date == "2026-01-03T10:00:00Z"], "TRUE")
  expect_true(is.na(res2$rev_a[res2$date == "2026-01-01T10:00:00Z"]))
})

test_that("an existing row is matched at most once, and equal comments keep one row each (A6-04)", {
  existing <- a6_existing(0:1, c("same words", "same words"), rev_a = c("TRUE", NA), note = c("first one", NA))
  res <- a6_merge(a6_incoming(0:1, c("same words", "same words")), existing)
  expect_equal(nrow(res), 2)
  expect_equal(sort(res$comment_id), c("0", "1"))
  expect_equal(res$rev_a[res$comment_id == "0"], "TRUE")
  expect_true(is.na(res$rev_a[res$comment_id == "1"]))

  # a third, new comment with the same words does not take a row twice
  res3 <- a6_merge(a6_incoming(0:2, rep("same words", 3)), existing)
  expect_equal(nrow(res3), 3)
  expect_equal(sum(res3$rev_a == "TRUE", na.rm = TRUE), 1)
})

test_that("a date or paragraph change does not break the match, and an author missing on one side agrees (A6-04)", {
  existing <- a6_existing(0, "dated comment", rev_a = "TRUE", note = "ok", date = "2025-12-01T10:00:00Z")
  res <- a6_merge(a6_incoming(7, "dated comment", date = "2026-02-01T10:00:00Z", para = 40), existing)
  expect_equal(nrow(res), 1)
  expect_equal(res$rev_a, "TRUE")
  expect_equal(res$date, "2026-02-01T10:00:00Z")
  expect_equal(res$comment_id, "7")

  anon <- a6_existing(0, "anonymous comment", rev_a = "TRUE", author = NA)
  res2 <- a6_merge(a6_incoming(3, "anonymous comment"), anon)
  expect_equal(nrow(res2), 1)
  expect_equal(res2$rev_a, "TRUE")
  expect_equal(res2$author, "Ann")

  # a different author is a different comment
  other <- a6_existing(0, "dated comment", rev_a = "TRUE", note = "ok", author = "Bob")
  res3 <- a6_merge(a6_incoming(0, "dated comment"), other)
  expect_equal(nrow(res3), 2)
  expect_equal(res3$doc_status[res3$author == "Bob"], "Prior Round / Not in docx")
})

test_that("text compared without whitespace matches text an earlier version fused (A6-04)", {
  existing <- a6_existing(0, "First para.Second para.", rev_a = "TRUE", note = "ok")
  res <- a6_merge(a6_incoming(5, "First para. Second para."), existing)
  expect_equal(nrow(res), 1)
  expect_equal(res$rev_a, "TRUE")
  expect_equal(res$comment_text, "First para. Second para.")
})

test_that("a comment without text matches only the row with the same id, author and date (A6-04)", {
  existing <- a6_existing(4, "", rev_a = "TRUE")
  same <- a6_merge(a6_incoming(4, ""), existing)
  expect_equal(nrow(same), 1)
  expect_equal(same$rev_a, "TRUE")
  moved <- a6_merge(a6_incoming(9, ""), existing)
  expect_equal(nrow(moved), 2)
})

test_that("match_incoming_comments prefers an exact match over a looser one (A6-04)", {
  existing <- a6_existing(0:1, c("one", "one"), date = c("2026-01-01T10:00:00Z", "2026-01-09T10:00:00Z"))
  # the first incoming comment only loosely matches row 2; the second matches it exactly
  incoming <- a6_incoming(0:1, c("one", "one"), date = c("2026-05-05T10:00:00Z", "2026-01-09T10:00:00Z"))
  expect_equal(match_incoming_comments(incoming, existing), c(1L, 2L))
})

test_that("a reviewer's sign-off survives a round in which Word renumbered the comments (A6-04)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  docx <- file.path(d, "eda.docx")
  body <- function(n) paste0(vapply(seq_len(n) - 1L, function(i) a6_commented_para(i, paste("text", i)), ""), collapse = "")

  a6_docx(docx, body(2), comments = c(a6_comment(0, "first comment"), a6_comment(1, "second comment")))
  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "second comment",
               list(rev_a = "TRUE", rev_a_comment = "I checked the SECOND comment"))

  a6_docx(docx, body(2), comments = c(a6_comment(0, "second comment"), a6_comment(1, "first comment")))
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)
  second <- r2$comments[r2$comments$comment_text == "second comment", ]
  first <- r2$comments[r2$comments$comment_text == "first comment", ]
  expect_equal(second$comment_id, "0")
  expect_equal(second$rev_a, "TRUE")
  expect_equal(second$rev_a_comment, "I checked the SECOND comment")
  expect_equal(first$comment_id, "1")
  expect_true(is.na(first$rev_a))

  # nothing moves on a run that changes nothing
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(r3$comments$comment_text, r2$comments$comment_text)
  expect_equal(r3$comments$rev_a, r2$comments$rev_a)
})

test_that("rows that share a comment id keep their own sign-offs through the fork overlay (A6-04)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  docx <- file.path(d, "eda.docx")
  body <- paste0(a6_commented_para(0, "alpha"), a6_commented_para(1, "beta"))

  a6_docx(docx, body, comments = c(a6_comment(0, "first comment"), a6_comment(1, "old wording")))
  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "old wording",
               list(rev_a = "TRUE", rev_a_comment = "agreed with the old wording"))

  # round 2: id 1 now carries a different text; the old row stays as a prior-round row
  a6_docx(docx, body, comments = c(a6_comment(0, "first comment"), a6_comment(1, "new wording")))
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(sum(r2$comments$comment_id == "1"), 2)
  a6_set_cells(r2$paths$forks[["rev_a"]], "Comments", "comment_text", "new wording",
               list(rev_a = "TRUE", rev_a_comment = "agreed with the new wording"))

  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  old <- r3$comments[r3$comments$comment_text == "old wording", ]
  new <- r3$comments[r3$comments$comment_text == "new wording", ]
  expect_equal(old$rev_a_comment, "agreed with the old wording")
  expect_equal(old$doc_status, "Prior Round / Not in docx")
  expect_equal(new$rev_a_comment, "agreed with the new wording")
  expect_equal(new$doc_status, "Active")
})

# =============================================================================
# A6-05: AN EDIT MADE IN THE MASTER IS NOT UNDONE BY A FORK THAT LEFT THE CELL ALONE
# =============================================================================

# One comment (paragraph 1) and one suggested change (paragraph 2)
a6_one_comment_one_change <- function(path) {
  a6_docx(
    path,
    paste0(a6_commented_para(0, "alpha"), a6_para("Beta.", ins = "added beta")),
    comments = a6_comment(0, "first comment")
  )
}

a6_clear_cells <- function(xlsx, sheet, match_col, match_val, cols) {
  wb <- openxlsx::loadWorkbook(xlsx)
  df <- openxlsx::readWorkbook(wb, sheet, colNames = TRUE)
  row_i <- which(df[[match_col]] == match_val)[1] + 1L
  openxlsx::deleteData(wb, sheet, cols = which(names(df) %in% cols), rows = row_i, gridExpand = TRUE)
  openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)
}

a6_workbook_xml <- function(xlsx) {
  td <- tempfile("a6_xlsx_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(xlsx, files = "xl/workbook.xml", exdir = td)
  paste(readLines(file.path(td, "xl", "workbook.xml"), warn = FALSE), collapse = "")
}

test_that("a sign-off typed into the master workbook survives the next run (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$master, "Comments", "comment_text", "first comment",
               list(rev_a = "TRUE", rev_a_comment = "recorded by the coordinator"))
  a6_set_cells(r1$paths$master, "SuggestedChanges", "original_text", "Beta.",
               list(rev_a = "TRUE", rev_a_comment = "change accepted"))

  expect_no_warning(r2 <- cbe_docx_review_extract(d, output_dir = out_dir))
  expect_equal(r2$comments$rev_a, "TRUE")
  expect_equal(r2$comments$rev_a_comment, "recorded by the coordinator")
  expect_equal(r2$redlines$rev_a, "TRUE")
  expect_equal(r2$redlines$rev_a_comment, "change accepted")

  # the new master and the regenerated fork both carry the entries
  master <- a6_sheet(r2$paths$master, "Comments")
  expect_equal(master$rev_a_comment, "recorded by the coordinator")
  fork <- a6_sheet(r2$paths$forks[["rev_a"]], "Comments")
  expect_equal(fork$rev_a, "TRUE")
  expect_equal(a6_sheet(r2$paths$forks[["rev_a"]], "SuggestedChanges")$rev_a_comment, "change accepted")

  # and they stay through further runs
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(r3$comments$rev_a_comment, "recorded by the coordinator")
})

test_that("a fork edit and a master edit to different cells of one row are both kept (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "first comment", list(rev_a = "TRUE"))
  a6_set_cells(r1$paths$master, "Comments", "comment_text", "first comment", list(rev_a_comment = "noted in master"))

  expect_no_warning(r2 <- cbe_docx_review_extract(d, output_dir = out_dir))
  expect_equal(r2$comments$rev_a, "TRUE")
  expect_equal(r2$comments$rev_a_comment, "noted in master")
})

test_that("a reviewer's own edits to the fork still come back, including emptying a cell (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "first comment",
               list(rev_a = "TRUE", rev_a_comment = "looks right"))
  expect_no_warning(r2 <- cbe_docx_review_extract(d, output_dir = out_dir))
  expect_equal(r2$comments$rev_a, "TRUE")
  expect_equal(r2$comments$rev_a_comment, "looks right")

  # the reviewer takes the sign-off and the note back
  a6_clear_cells(r2$paths$forks[["rev_a"]], "Comments", "comment_text", "first comment", c("rev_a", "rev_a_comment"))
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_true(is.na(r3$comments$rev_a))
  expect_true(is.na(r3$comments$rev_a_comment))
})

test_that("a cell changed in both the master and the fork takes the fork's value and warns (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$master, "Comments", "comment_text", "first comment", list(rev_a_comment = "master note"))
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "first comment", list(rev_a_comment = "fork note"))

  expect_warning(
    r2 <- cbe_docx_review_extract(d, output_dir = out_dir),
    "both in the master workbook and in a reviewer's fork.*rev_a_comment of rev_a: master 'master note', fork 'fork note'"
  )
  expect_equal(r2$comments$rev_a_comment, "fork note")
  # the replaced master is in the backups
  backups <- list.files(file.path(out_dir, "backups"), pattern = "^review_tracker_[0-9].*\\.bak$", full.names = TRUE)
  expect_true(length(backups) >= 1)
})

test_that("a fork without a baseline (an earlier version's) never blanks a master cell (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  for (fork in r1$paths$forks) {
    wb <- openxlsx::loadWorkbook(fork)
    openxlsx::removeWorksheet(wb, "ForkBaseline")
    openxlsx::saveWorkbook(wb, fork, overwrite = TRUE)
  }
  a6_set_cells(r1$paths$master, "Comments", "comment_text", "first comment",
               list(rev_a = "TRUE", rev_a_comment = "typed in the master"))
  a6_set_cells(r1$paths$forks[["rev_b"]], "Comments", "comment_text", "first comment", list(rev_b = "TRUE"))

  expect_no_warning(r2 <- cbe_docx_review_extract(d, output_dir = out_dir))
  expect_equal(r2$comments$rev_a_comment, "typed in the master")
  expect_equal(r2$comments$rev_a, "TRUE")
  expect_equal(r2$comments$rev_b, "TRUE")
})

test_that("apply_reviewer_fork_overlays leaves a master cell alone when the fork cell is blank (A6-05)", {
  skip_if_not_installed("openxlsx")
  d <- a6_dir()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Comments")
  openxlsx::writeData(wb, "Comments", data.frame(
    file = c("eda.docx", "eda.docx", "eda.docx"), comment_id = c("1", "2", "3"),
    comment_text = c("one", "two", "three"),
    rev_a = c(NA, "TRUE", "FALSE"), rev_a_comment = c(NA, NA, "fork says no"),
    stringsAsFactors = FALSE
  ))
  openxlsx::saveWorkbook(wb, file.path(d, "review_tracker_rev_a.xlsx"))

  master <- tibble::tibble(
    file = "eda.docx", comment_id = c("1", "2", "3"), comment_text = c("one", "two", "three"),
    rev_a = c("TRUE", NA, "TRUE"), rev_a_comment = c("master note", "master note", NA)
  )
  expect_warning(
    res <- apply_reviewer_fork_overlays(d, "review_tracker.xlsx", master, tibble::tibble(), "rev_a")$comments,
    "1 cell\\(s\\) were changed both"
  )
  # blank fork cells do not wipe the master's
  expect_equal(res$rev_a[1], "TRUE")
  expect_equal(res$rev_a_comment[1], "master note")
  expect_equal(res$rev_a_comment[2], "master note")
  # filled fork cells are taken; a different fork value replaces a master value (reported above)
  expect_equal(res$rev_a[2], "TRUE")
  expect_equal(res$rev_a[3], "FALSE")
  expect_equal(res$rev_a_comment[3], "fork says no")
})

test_that("the baseline sheet of a fork is hidden and holds only that reviewer's cells (A6-05)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_one_comment_one_change(file.path(d, "eda.docx"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$master, "Comments", "comment_text", "first comment",
               list(rev_b = "TRUE", rev_b_comment = "PRIVATE-NOTE-OF-B"))
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)

  fork_a <- r2$paths$forks[["rev_a"]]
  expect_match(a6_workbook_xml(fork_a), "<sheet name=\"ForkBaseline\"[^>]*state=\"veryHidden\"")
  baseline <- a6_sheet(fork_a, "ForkBaseline")
  expect_false(any(grepl("PRIVATE-NOTE-OF-B", unlist(baseline))))
  expect_setequal(unique(baseline$table), c("Comments", "SuggestedChanges"))
  # the master has none
  expect_false("ForkBaseline" %in% readxl::excel_sheets(r2$paths$master))
})

# =============================================================================
# A6-06: DOCUMENT-LEVEL SIGN-OFFS ON THE Documents_<id> SHEETS
# =============================================================================

test_that("load_existing_tracker reads every Documents sheet and merges the rows by file (A6-06)", {
  skip_if_not_installed("openxlsx")
  d <- a6_dir()
  path <- file.path(d, "review_tracker.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Documents")
  openxlsx::writeData(wb, "Documents", data.frame(file = c("a.docx", "b.docx"), comment_count = c(2, 1), resolved = c(FALSE, TRUE)))
  openxlsx::addWorksheet(wb, "Documents_lead")
  openxlsx::writeData(wb, "Documents_lead", data.frame(
    file = c("a.docx", "b.docx"), pipeline_stage = "s", lead = c("TRUE", NA), lead_comment = c("approved", NA),
    stringsAsFactors = FALSE
  ))
  openxlsx::addWorksheet(wb, "Documents_second")
  openxlsx::writeData(wb, "Documents_second", data.frame(file = "b.docx", second = "FALSE", stringsAsFactors = FALSE))
  openxlsx::saveWorkbook(wb, path)

  docs <- load_existing_tracker(path)$documents
  expect_setequal(names(docs), c("a.docx", "b.docx"))
  expect_equal(docs[["a.docx"]]$lead, "TRUE")
  expect_equal(docs[["a.docx"]]$lead_comment, "approved")
  expect_equal(docs[["a.docx"]]$comment_count, "2")
  expect_equal(docs[["b.docx"]]$second, "FALSE")
  expect_true(is.na(docs[["b.docx"]]$lead))
})

test_that("a sign-off typed on a Documents_<id> sheet survives the next run (A6-06)", {
  skip_if_not_installed("openxlsx")
  a6_options(fork = "rev_a", signoff = c("lead", "second"), mode = "per_reviewer")
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_docx(file.path(d, "eda.docx"), a6_commented_para(0, "alpha"), comments = a6_comment(0, "only comment"))
  a6_docx(file.path(d, "intro.docx"), a6_commented_para(0, "intro"), comments = a6_comment(0, "intro comment"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$master, "Documents_lead", "file", "eda.docx",
               list(lead = "TRUE", lead_comment = "LEAD-SIGNOFF-NOTE"))
  a6_set_cells(r1$paths$master, "Documents_second", "file", "intro.docx",
               list(second = "FALSE", second_comment = "needs another look"))

  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)
  lead <- a6_sheet(r2$paths$master, "Documents_lead")
  expect_equal(lead$lead[lead$file == "eda.docx"], "TRUE")
  expect_equal(lead$lead_comment[lead$file == "eda.docx"], "LEAD-SIGNOFF-NOTE")
  second <- a6_sheet(r2$paths$master, "Documents_second")
  expect_equal(second$second_comment[second$file == "intro.docx"], "needs another look")
  expect_equal(r2$documents$lead[r2$documents$file == "eda.docx"], TRUE)
  expect_equal(r2$documents$lead_comment[r2$documents$file == "eda.docx"], "LEAD-SIGNOFF-NOTE")

  # and through yet another run
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  lead3 <- a6_sheet(r3$paths$master, "Documents_lead")
  expect_equal(lead3$lead_comment[lead3$file == "eda.docx"], "LEAD-SIGNOFF-NOTE")
})

test_that("document sign-offs follow a change of documents_sheet_mode in either direction (A6-06)", {
  skip_if_not_installed("openxlsx")
  a6_options(fork = "rev_a", signoff = "lead", mode = "per_reviewer")
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_docx(file.path(d, "eda.docx"), a6_commented_para(0, "alpha"), comments = a6_comment(0, "only comment"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  a6_set_cells(r1$paths$master, "Documents_lead", "file", "eda.docx",
               list(lead = "TRUE", lead_comment = "signed off in per-reviewer mode"))

  options(review.documents_sheet_mode = "columns")
  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)
  docs <- a6_sheet(r2$paths$master, "Documents")
  expect_equal(docs$lead, "TRUE")
  expect_equal(docs$lead_comment, "signed off in per-reviewer mode")
  expect_false("Documents_lead" %in% readxl::excel_sheets(r2$paths$master))

  options(review.documents_sheet_mode = "per_reviewer")
  r3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  lead <- a6_sheet(r3$paths$master, "Documents_lead")
  expect_equal(lead$lead_comment, "signed off in per-reviewer mode")
})

# =============================================================================
# A6-08 / A6-10: DOCUMENTS THAT CANNOT BE READ
# =============================================================================

# Evaluate `expr` and return its value together with every warning it raised
a6_with_warnings <- function(expr) {
  warnings <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

test_that("a document that fails to extract keeps what the tracker holds for it (A6-08)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  eda <- file.path(d, "eda.docx")
  eda_body <- paste0(
    a6_commented_para(0, "alpha"), a6_commented_para(1, "beta"), a6_commented_para(2, "gamma"),
    a6_para("Delta.", ins = "added delta")
  )
  eda_comments <- c(a6_comment(0, "first comment"), a6_comment(1, "second comment"), a6_comment(2, "third comment"))
  a6_docx(eda, eda_body, comments = eda_comments)
  a6_docx(file.path(d, "intro.docx"), a6_commented_para(0, "intro"), comments = a6_comment(0, "intro comment"))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(sum(r1$comments$file == "eda.docx"), 3)
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "second comment", list(rev_a = "TRUE"))

  # round 2: eda.docx arrives with a truncated word/document.xml, which still passes as a package
  a6_docx(eda, document_xml = "<w:document><oops", comments = eda_comments)
  r2 <- a6_with_warnings(cbe_docx_review_extract(d, output_dir = out_dir))
  res <- r2$value
  expect_true(any(grepl("1 Word file\\(s\\) could not be read.*eda\\.docx", r2$warnings)))
  expect_equal(res$errors$file, "eda.docx")

  eda_rows <- res$comments[res$comments$file == "eda.docx", ]
  expect_equal(nrow(eda_rows), 3)
  expect_true(all(eda_rows$doc_status == "Prior Round / Not in docx"))
  expect_equal(eda_rows$rev_a[eda_rows$comment_text == "second comment"], "TRUE")
  expect_equal(sum(res$revisions$file == "eda.docx"), 1)
  expect_equal(sum(res$redlines$file == "eda.docx"), 1)
  expect_equal(sum(res$comments$file == "intro.docx"), 1)

  # the Documents sheet reports the rows it still holds, and the workbook lists the error
  docs <- a6_sheet(res$paths$master, "Documents")
  expect_equal(docs$comment_count[docs$file == "eda.docx"], "3")
  expect_equal(a6_sheet(res$paths$master, "Errors")$file, "eda.docx")

  # round 3: the document is repaired and its rows are continued, sign-off included
  a6_docx(eda, eda_body, comments = eda_comments)
  res3 <- cbe_docx_review_extract(d, output_dir = out_dir)
  eda3 <- res3$comments[res3$comments$file == "eda.docx", ]
  expect_equal(nrow(eda3), 3)
  expect_true(all(eda3$doc_status == "Active"))
  expect_equal(eda3$rev_a[eda3$comment_text == "second comment"], "TRUE")
  expect_equal(nrow(res3$errors), 0)
})

test_that("files that are not readable Word documents are reported, and lock files are skipped quietly (A6-10)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  a6_docx(file.path(d, "intro.docx"), a6_commented_para(0, "intro"), comments = a6_comment(0, "intro comment"))
  file.create(file.path(d, "empty.docx"))
  writeLines("not a zip archive at all", file.path(d, "notzip.docx"))
  writeBin(as.raw(sample(0:255, 162, replace = TRUE)), file.path(d, "~$intro.docx"))
  a6_docx(file.path(d, "renamed.docx"), document = FALSE, parts = list("xl/workbook.xml" = "<workbook/>"))

  r <- a6_with_warnings(cbe_docx_review_extract(d, output_dir = out_dir))
  res <- r$value

  # one warning, with the count, naming the files but not the lock file
  expect_length(r$warnings, 1)
  expect_match(r$warnings, "3 Word file\\(s\\) could not be read")
  expect_match(r$warnings, "empty\\.docx")
  expect_false(grepl("~\\$intro", r$warnings, fixed = FALSE))

  expect_setequal(res$errors$file, c("empty.docx", "notzip.docx", "renamed.docx"))
  expect_false(any(grepl("~$", res$errors$file, fixed = TRUE)))
  expect_true(all(nzchar(res$errors$error)))
  expect_equal(res$errors$error[res$errors$file == "empty.docx"], "Empty file")
  expect_match(res$errors$error[res$errors$file == "renamed.docx"], "word/document.xml is missing", fixed = TRUE)
  # no path or user name in what is stored
  expect_false(any(grepl(d, res$errors$error, fixed = TRUE)))

  # only the readable document is a document of the run
  expect_equal(res$documents$file, "intro.docx")
  expect_equal(nrow(res$comments), 1)
  expect_setequal(a6_sheet(res$paths$master, "Errors")$file, c("empty.docx", "notzip.docx", "renamed.docx"))
})

test_that("a folder with no readable Word document warns, reports why, and writes nothing (A6-10)", {
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  file.create(file.path(d, "empty.docx"))
  writeLines("text", file.path(d, "notzip.docx"))

  r <- a6_with_warnings(cbe_docx_review_extract(d, output_dir = out_dir))
  expect_length(r$warnings, 1)
  expect_match(r$warnings, "No valid DOCX files found to process: 2 file\\(s\\) could not be read")
  expect_setequal(r$value$errors$file, c("empty.docx", "notzip.docx"))
  expect_false(dir.exists(out_dir))
})

test_that("docx_problem names why a file is not a readable document (A6-10)", {
  d <- a6_dir()
  good <- file.path(d, "good.docx")
  a6_docx(good)
  expect_true(is.na(docx_problem(good)))
  expect_true(is_valid_docx(good))

  expect_equal(docx_problem(file.path(d, "missing.docx")), "File not found")
  file.create(file.path(d, "empty.docx"))
  expect_equal(docx_problem(file.path(d, "empty.docx")), "Empty file")
  writeLines("plain text", file.path(d, "text.docx"))
  expect_match(docx_problem(file.path(d, "text.docx")), "Not a readable ZIP archive")

  a6_docx(file.path(d, "sheet.docx"), document = FALSE, parts = list("xl/workbook.xml" = "<workbook/>"))
  expect_match(docx_problem(file.path(d, "sheet.docx")), "word/document.xml is missing", fixed = TRUE)
  expect_false(is_valid_docx(file.path(d, "sheet.docx")))
})

test_that("a run that finds no Word document creates and backs up nothing (A6-25)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  writeLines("notes", file.path(d, "notes.txt"))
  file.create(file.path(d, "~$lock.docx"))

  # the default input folder is the working directory
  withr::local_dir(d)
  res <- expect_no_warning(cbe_docx_review_extract())
  expect_equal(nrow(res$comments), 0)
  expect_equal(sort(list.files(d, all.files = TRUE, no.. = TRUE)), sort(c("notes.txt", "~$lock.docx")))

  # an existing tracker is not backed up when nothing is written either
  out_dir <- file.path(d, "elsewhere")
  dir.create(out_dir)
  file.copy(system.file("DESCRIPTION", package = "TempleCBE"), file.path(out_dir, "review_tracker.xlsx"))
  cbe_docx_review_extract(d, output_dir = out_dir)
  expect_false(dir.exists(file.path(out_dir, "backups")))
})

# =============================================================================
# A6-20: PARAGRAPHS, TABS AND BREAKS DO NOT FUSE WORDS
# =============================================================================

test_that("the paragraphs of a comment and of a selection are separated, not fused (A6-20)", {
  d <- a6_dir()
  docx <- file.path(d, "multi.docx")
  body <- paste0(
    a6_para("P1 intro."),
    '<w:p><w:commentRangeStart w:id="5"/><w:r><w:t>P2 start</w:t></w:r></w:p>',
    '<w:p><w:r><w:t>P3 end</w:t></w:r><w:commentRangeEnd w:id="5"/></w:p>'
  )
  a6_docx(docx, body, comments = a6_comment(5, c("First para.", "Second para.")))
  res <- extract_from_docx(docx)

  expect_equal(res$comments$comment_text, "First para. Second para.")
  expect_equal(res$comments$selected_text, "P2 start P3 end")
  expect_equal(res$comments$paragraph_context, "P2 start P3 end")
})

test_that("tabs, line breaks and non-breaking hyphens in the text of a document are kept as separators (A6-20)", {
  d <- a6_dir()
  docx <- file.path(d, "runs.docx")
  body <- paste0(
    # a tracked insertion with a tab inside it, in a paragraph with a tab stop and a break
    '<w:p><w:pPr><w:tabs><w:tab w:val="left" w:pos="720"/></w:tabs></w:pPr>',
    '<w:r><w:t>Hello</w:t></w:r><w:r><w:tab/></w:r><w:r><w:t>World</w:t></w:r>',
    '<w:r><w:br/></w:r><w:r><w:t>Next</w:t></w:r>',
    '<w:ins w:id="1" w:author="Cat" w:date="2026-01-02T10:00:00Z">',
    '<w:r><w:t>added</w:t></w:r><w:r><w:tab/></w:r><w:r><w:t>words</w:t></w:r></w:ins></w:p>',
    # a tab stop alone must not add a space, a non-breaking hyphen is a hyphen
    '<w:p><w:pPr><w:tabs><w:tab w:val="left" w:pos="720"/></w:tabs></w:pPr>',
    '<w:r><w:t>A</w:t></w:r><w:r><w:t>B</w:t></w:r>',
    '<w:r><w:t>e</w:t></w:r><w:r><w:noBreakHyphen/></w:r><w:r><w:t>mail</w:t></w:r></w:p>',
    # a commented selection that contains a tab
    '<w:p><w:commentRangeStart w:id="1"/><w:r><w:t>left</w:t></w:r><w:r><w:tab/></w:r><w:r><w:t>right</w:t></w:r><w:commentRangeEnd w:id="1"/></w:p>'
  )
  a6_docx(docx, body, comments = a6_comment(1, "note"))
  res <- extract_from_docx(docx)

  expect_equal(res$redlines$original_text[res$redlines$paragraph_number == 1], "Hello World Next")
  expect_equal(res$redlines$accepted_text[res$redlines$paragraph_number == 1], "Hello World Nextadded words")
  expect_equal(res$revisions$changed_text, "added words")
  expect_equal(res$revisions$paragraph_context, "Hello World Nextadded words")
  expect_equal(res$comments$selected_text, "left right")

  # the text rules on a bare paragraph: a tab stop adds nothing, a non-breaking hyphen is a hyphen
  doc <- xml2::read_xml(paste0(
    '<w:document xmlns:w="', A6_W, '"><w:body>',
    '<w:p><w:pPr><w:tabs><w:tab w:val="left"/></w:tabs></w:pPr><w:r><w:t>A</w:t></w:r><w:r><w:t>B</w:t></w:r>',
    '<w:r><w:t>e</w:t></w:r><w:r><w:noBreakHyphen/></w:r><w:r><w:t>mail</w:t></w:r></w:p></w:body></w:document>'
  ))
  p <- xml2::xml_find_first(doc, "//w:p", XML_NAMESPACES)
  expect_equal(clean_review_text(node_text(p)), "ABe-mail")
})

test_that("rows and a tracker written by an earlier version, with fused words, still match (A6-20, A6-04)", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out_dir <- file.path(d, "out")
  docx <- file.path(d, "eda.docx")
  body <- paste0(
    '<w:p><w:commentRangeStart w:id="0"/><w:r><w:t>one</w:t></w:r><w:commentRangeEnd w:id="0"/></w:p>',
    '<w:p><w:r><w:t>Hello</w:t></w:r><w:r><w:tab/></w:r><w:r><w:t>World</w:t></w:r>',
    '<w:ins w:id="2" w:author="Cat" w:date="2026-01-02T10:00:00Z"><w:r><w:t> more</w:t></w:r></w:ins></w:p>'
  )
  a6_docx(docx, body, comments = a6_comment(0, c("First para.", "Second para.")))

  r1 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(r1$comments$comment_text, "First para. Second para.")
  expect_equal(r1$redlines$original_text, "Hello World")

  # what the extractor wrote before this fix: the words fused, with sign-offs entered on top
  wb <- openxlsx::loadWorkbook(r1$paths$master)
  cc <- names(openxlsx::readWorkbook(wb, "Comments"))
  openxlsx::writeData(wb, "Comments", "First para.Second para.", startCol = which(cc == "comment_text"), startRow = 2)
  openxlsx::writeData(wb, "Comments", "TRUE", startCol = which(cc == "rev_a"), startRow = 2)
  openxlsx::writeData(wb, "Comments", "checked", startCol = which(cc == "rev_a_comment"), startRow = 2)
  rc <- names(openxlsx::readWorkbook(wb, "SuggestedChanges"))
  openxlsx::writeData(wb, "SuggestedChanges", "HelloWorld", startCol = which(rc == "original_text"), startRow = 2)
  openxlsx::writeData(wb, "SuggestedChanges", "TRUE", startCol = which(rc == "rev_a"), startRow = 2)
  openxlsx::saveWorkbook(wb, r1$paths$master, overwrite = TRUE)
  # (the forks still hold the new text; remove them so only the master speaks)
  file.remove(unlist(r1$paths$forks))

  r2 <- cbe_docx_review_extract(d, output_dir = out_dir)
  expect_equal(nrow(r2$comments), 1)
  expect_equal(r2$comments$rev_a, "TRUE")
  expect_equal(r2$comments$rev_a_comment, "checked")
  expect_equal(r2$comments$comment_text, "First para. Second para.")
  expect_equal(nrow(r2$redlines), 1)
  expect_equal(r2$redlines$rev_a, "TRUE")
  expect_equal(r2$redlines$original_text, "Hello World")
  expect_equal(r2$comments$doc_status, "Active")
})

# =============================================================================
# A6-25: BACKUPS, ORDERING, review_config(reset = TRUE)
# =============================================================================

test_that("backups made within the same second all survive (A6-25)", {
  d <- a6_dir()
  path <- file.path(d, "review_tracker.xlsx")
  made <- character(0)
  for (i in 1:8) {
    writeLines(paste("version", i), path)
    made <- c(made, backup_review_file(path))
  }
  expect_length(unique(made), 8)
  expect_length(list.files(file.path(d, "backups")), 8)
  # each backup holds the file as it was when it was made, the first one included
  expect_equal(readLines(made[1]), "version 1")
  expect_equal(readLines(made[8]), "version 8")
  expect_null(backup_review_file(file.path(d, "no_such_file.xlsx")))
})

test_that("rows and files are ordered by plain byte order whatever the locale (A6-25)", {
  # testthat sorts in the C locale; use a locale that collates "alpha" before "Zeta", where one exists
  old_collate <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old_collate), add = TRUE)
  for (loc in c("en_US.UTF-8", "en_US.utf8", "English_United States.1252")) {
    suppressWarnings(Sys.setlocale("LC_COLLATE", loc))
    if (identical(sort(c("Zeta", "alpha")), c("alpha", "Zeta"))) break
  }
  skip_if_not(
    identical(sort(c("Zeta", "alpha")), c("alpha", "Zeta")),
    "no locale-aware collation available on this machine"
  )
  catalog <- load_pipeline_catalog(NULL)
  files <- c("alpha_notes.docx", "Zeta_notes.docx")
  expect_true(all(vapply(files, function(f) match_docx_to_pipeline(f, catalog)$rank, integer(1)) == 999L))

  incoming <- do.call(rbind, lapply(files, function(f) {
    tibble::tibble(
      file = f, comment_id = "0", author = "Ann", date = "2026-01-01T10:00:00Z", comment_text = paste("note in", f),
      selected_text = "", paragraph_number = 1L, end_paragraph_number = NA_integer_, paragraph_context = "",
      resolved_in_docx = FALSE, is_reply = FALSE, reply_to_id = NA_character_
    )
  }))
  res <- merge_comments(incoming, tibble::tibble(), character(0), catalog, files,
                        fork_reviewers = "rev_a", signoff_reviewers = character(0))$rows
  # "Z" sorts before "a" bytewise; a locale-aware sort would put "alpha" first
  expect_equal(res$file, c("Zeta_notes.docx", "alpha_notes.docx"))

  rev <- tibble::tibble(
    file = files, revision_type = "insertion", author = "Cat", date = "d", changed_text = "xx",
    paragraph_number = 1L, paragraph_context = "c"
  )
  expect_equal(merge_revisions(rev, tibble::tibble(), catalog, files)$file, c("Zeta_notes.docx", "alpha_notes.docx"))
})

test_that("review_config(reset = TRUE) returns to the defaults (A6-25)", {
  old <- options(
    review.fork_reviewers = NULL, review.signoff_reviewers = NULL, review.documents_sheet_mode = NULL
  )
  on.exit(options(old), add = TRUE)

  old_more <- options(
    review.csv_reviewer_columns = NULL, review.pipeline_catalog = NULL, review.stem_aliases = NULL,
    review.analysis_dir = NULL, review.input_dirs = NULL
  )
  on.exit(options(old_more), add = TRUE)
  review_config(fork_reviewers = c("x", "y"), signoff_reviewers = "lead", documents_sheet_mode = "per_reviewer",
                csv_reviewer_columns = "all", stem_aliases = c(typo_stem = "right_stem"), input_dirs = "some_folder")
  expect_equal(review_config()$fork_reviewers, c("x", "y"))
  expect_equal(review_config()$csv_reviewer_columns, "all")

  cfg <- review_config(reset = TRUE)
  expect_equal(cfg$csv_reviewer_columns, "fork")
  expect_equal(cfg$stem_aliases, character(0))
  expect_equal(cfg$input_dirs, character(0))
  expect_equal(cfg$fork_reviewers, paste0("reviewer_", 1:3))
  expect_equal(cfg$signoff_reviewers, character(0))
  expect_equal(cfg$documents_sheet_mode, "columns")
  expect_equal(review_config(), cfg)
  expect_null(getOption("review.fork_reviewers"))

  # arguments given with reset apply after it
  cfg2 <- review_config(fork_reviewers = "solo", reset = TRUE)
  expect_equal(cfg2$fork_reviewers, "solo")
  expect_equal(cfg2$signoff_reviewers, character(0))
  expect_false(isTRUE(formals(review_config)$reset))
})
