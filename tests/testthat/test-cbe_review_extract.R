# Unit tests for cbe_docx_review_extract and supporting functions

# =============================================================================
# SYNTHETIC DOCX HELPER
# =============================================================================

create_synthetic_docx <- function(target_path,
                                  document_xml_content = NULL,
                                  comments_xml_content = NULL) {
  td <- tempfile(pattern = "synth_docx_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # [Content_Types].xml
  content_types <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">\n',
    '  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>\n',
    '  <Default Extension="xml" ContentType="application/xml"/>\n',
    '  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>\n',
    if (!is.null(comments_xml_content)) '  <Override PartName="/word/comments.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/>\n' else '',
    '</Types>'
  )
  writeLines(content_types, file.path(td, "[Content_Types].xml"), useBytes = TRUE)

  word_dir <- file.path(td, "word")
  dir.create(word_dir, recursive = TRUE, showWarnings = FALSE)

  # word/document.xml
  if (is.null(document_xml_content)) {
    document_xml_content <- paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
      '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
      '  <w:body>\n',
      '    <w:p><w:r><w:t>Hello world</w:t></w:r></w:p>\n',
      '  </w:body>\n',
      '</w:document>'
    )
  }
  writeLines(document_xml_content, file.path(word_dir, "document.xml"), useBytes = TRUE)

  # word/comments.xml
  if (!is.null(comments_xml_content)) {
    writeLines(comments_xml_content, file.path(word_dir, "comments.xml"), useBytes = TRUE)
  }

  # Zip into target_path
  old_wd <- getwd()
  setwd(td)
  on.exit(setwd(old_wd), add = TRUE)

  all_files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (file.exists(target_path)) unlink(target_path)
  utils::zip(target_path, files = all_files, flags = "-r9Xq")
  invisible(target_path)
}

# =============================================================================
# TESTS: HELPER FUNCTIONS
# =============================================================================

test_that("clean_review_text normalizes unicode, mojibake, quotes, and whitespace", {
  expect_equal(clean_review_text(NULL), "")
  expect_equal(clean_review_text(NA_character_), "")
  expect_equal(clean_review_text(""), "")

  # Smart quotes and dashes
  raw <- "Here\u00a0is \u2018single\u2019 and \u201cdouble\u201d quotes \u2013 with \u2014 dashes\u2026   extra   spaces"
  cleaned <- clean_review_text(raw)
  expect_equal(cleaned, "Here is 'single' and \"double\" quotes - with - dashes... extra spaces")

  # Mojibake sequences
  mojibake <- "Mojibake \u00e2\u20ac\u0153test\u00e2\u20ac\u009d and \u00e2\u20ac\u2013 dash"
  expect_equal(clean_review_text(mojibake), "Mojibake \"test\" and - dash")
})

test_that("extract_docx_stem strips extensions, initials, and dates", {
  expect_equal(extract_docx_stem("eda_MRW_11_21_25.docx"), "eda")
  expect_equal(extract_docx_stem("intro_JKA_12_5_2025.docx"), "intro")
  expect_equal(extract_docx_stem("analysis_12_5_25.docx"), "analysis")
  expect_equal(extract_docx_stem("single_var_mixed_models.docx"), "single_var_mixed_models")
  expect_equal(extract_docx_stem("missing_data_imputat_non_repeat.docx"), "missing_data_imputat_non_repeat")
})

test_that("load_pipeline_catalog and match_docx_to_pipeline resolve stages", {
  catalog <- load_pipeline_catalog(NULL)
  expect_true(length(catalog) >= 38)
  expect_true("intro" %in% names(catalog))
  expect_true("eda" %in% names(catalog))

  # Exact match
  m1 <- match_docx_to_pipeline("intro_11_21_25.docx", catalog)
  expect_equal(m1$matched_stem, "intro")
  expect_equal(m1$stage, "Heading 1: Introduction")
  expect_equal(m1$rank, 0L)

  # Alias match
  m2 <- match_docx_to_pipeline("miss_data_imputat_non_repeat.docx", catalog)
  expect_equal(m2$matched_stem, "missing_data_imputation_non_repeat")

  # Fuzzy match
  m3 <- match_docx_to_pipeline("eda_impute_typo.docx", catalog)
  expect_equal(m3$matched_stem, "eda_impute")

  # Unknown file
  m4 <- match_docx_to_pipeline("completely_unrelated_file.docx", catalog)
  expect_equal(m4$rank, 999L)
  expect_equal(m4$stage, "Unknown / Extra")
})

# =============================================================================
# TESTS: DOCX XML EXTRACTION
# =============================================================================

test_that("extract_from_docx extracts comments, selected text, and replies", {
  tmp_dir <- tempfile(pattern = "test_extract_docx_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  docx_file <- file.path(tmp_dir, "test_doc.docx")

  doc_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:r><w:t>First paragraph with </w:t></w:r>\n',
    '      <w:commentRangeStart w:id="1"/>\n',
    '      <w:r><w:t>highlighted target</w:t></w:r>\n',
    '      <w:commentRangeEnd w:id="1"/>\n',
    '      <w:r><w:t> text.</w:t></w:r>\n',
    '    </w:p>\n',
    '    <w:p>\n',
    '      <w:r><w:t>Second paragraph.</w:t></w:r>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )

  comments_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="1" w:author="Dr. Wolfson" w:date="2025-11-21T10:00:00Z" w:done="0">\n',
    '    <w:p><w:r><w:t>Please clarify this finding.</w:t></w:r></w:p>\n',
    '    <w:comment w:id="2" w:author="Kyle" w:date="2025-11-21T11:00:00Z" w:done="1">\n',
    '      <w:p><w:r><w:t>Updated in section 3.</w:t></w:r></w:p>\n',
    '    </w:comment>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )

  create_synthetic_docx(docx_file, doc_xml, comments_xml)

  res <- extract_from_docx(docx_file)
  expect_equal(nrow(res$comments), 2)

  # Check top-level comment
  c1 <- res$comments[res$comments$comment_id == "1", ]
  expect_equal(c1$author, "Dr. Wolfson")
  expect_equal(c1$comment_text, "Please clarify this finding.")
  expect_equal(c1$selected_text, "highlighted target")
  expect_equal(c1$paragraph_number, 1L)
  expect_false(c1$is_reply)
  expect_false(c1$resolved_in_docx)

  # Check reply comment
  c2 <- res$comments[res$comments$comment_id == "2", ]
  expect_equal(c2$author, "Kyle")
  expect_equal(c2$comment_text, "Updated in section 3.")
  expect_true(c2$is_reply)
  expect_equal(c2$reply_to_id, "1")
  expect_true(c2$resolved_in_docx)
})

test_that("extract_from_docx extracts revisions and paragraph redlines", {
  tmp_dir <- tempfile(pattern = "test_redlines_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  docx_file <- file.path(tmp_dir, "redline_doc.docx")

  doc_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:r><w:t>This is the </w:t></w:r>\n',
    '      <w:del w:author="Editor" w:date="2025-11-20T09:00:00Z"><w:r><w:delText>old</w:delText></w:r></w:del>\n',
    '      <w:ins w:author="Editor" w:date="2025-11-20T09:00:00Z"><w:r><w:t>new</w:t></w:r></w:ins>\n',
    '      <w:r><w:t> version of text.</w:t></w:r>\n',
    '    </w:p>\n',
    '    <w:p>\n',
    '      <w:pPr><w:pStyle w:val="TOC1"/></w:pPr>\n',
    '      <w:r><w:t>Contents Entry </w:t></w:r>\n',
    '      <w:ins w:author="Word" w:date="2025-11-20T09:00:00Z"><w:r><w:t>5</w:t></w:r></w:ins>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )

  create_synthetic_docx(docx_file, doc_xml)

  res <- extract_from_docx(docx_file, min_revision_length = 1)
  expect_equal(nrow(res$revisions), 3) # 1 del, 1 ins, 1 ins in TOC
  expect_equal(nrow(res$redlines), 2)

  # Verify reconstructed original and accepted text
  rl1 <- res$redlines[res$redlines$paragraph_number == 1, ]
  expect_equal(rl1$original_text, "This is the old version of text.")
  expect_equal(rl1$accepted_text, "This is the new version of text.")
  expect_false(rl1$is_toc_or_lof)
  expect_equal(rl1$author, "Editor")

  # Verify TOC entry is flagged
  rl2 <- res$redlines[res$redlines$paragraph_number == 2, ]
  expect_true(rl2$is_toc_or_lof)
})

# =============================================================================
# TESTS: NON-DESTRUCTIVE MERGE ENGINE
# =============================================================================

test_that("merge_comments preserves human reviewer sign-offs and custom columns", {
  catalog <- load_pipeline_catalog(NULL)

  incoming <- tibble::tibble(
    file = "intro.docx",
    comment_id = "1",
    author = "Dr. Wolfson",
    date = "2025-11-21",
    comment_text = "Check p-value.",
    selected_text = "p < 0.05",
    paragraph_number = 3L,
    end_paragraph_number = NA_integer_,
    paragraph_context = "Context here.",
    resolved_in_docx = FALSE,
    is_reply = FALSE,
    reply_to_id = NA_character_
  )

  existing <- tibble::tibble(
    file = "intro.docx",
    comment_id = "1",
    author = "Dr. Wolfson",
    date = "2025-11-20",
    comment_text = "Old comment text",
    jka = "TRUE",
    jka_comment = "Verified in code",
    darina = "TRUE",
    darina_comment = "Agreed",
    custom_col = "Important Note"
  )

  res <- merge_comments(
    incoming_comments = incoming,
    existing_rows = existing,
    existing_headers = names(existing),
    catalog = catalog,
    scanned_files = "intro.docx"
  )

  merged_df <- res$rows
  expect_equal(nrow(merged_df), 1)
  expect_equal(merged_df$comment_text[1], "Check p-value.")
  expect_equal(merged_df$jka[1], "TRUE")
  expect_equal(merged_df$jka_comment[1], "Verified in code")
  expect_equal(merged_df$darina[1], "TRUE")
  expect_equal(merged_df$custom_col[1], "Important Note")
  expect_equal(merged_df$doc_status[1], "Active")
})

test_that("merge_comments marks unreviewed removed comments as Prior Round", {
  catalog <- load_pipeline_catalog(NULL)

  # No incoming comments
  incoming <- tibble::tibble()

  # Existing comment with human feedback
  existing <- tibble::tibble(
    file = "intro.docx",
    comment_id = "99",
    comment_text = "Deleted from docx but reviewed",
    jka = "TRUE",
    jka_comment = "Saved answer"
  )

  res <- merge_comments(
    incoming_comments = incoming,
    existing_rows = existing,
    existing_headers = names(existing),
    catalog = catalog,
    scanned_files = "intro.docx"
  )

  expect_equal(nrow(res$rows), 1)
  expect_equal(res$rows$doc_status[1], "Prior Round / Not in docx")
  expect_equal(res$rows$jka[1], "TRUE")
})

# =============================================================================
# TESTS: REVIEWER FORK OVERLAYS
# =============================================================================

test_that("apply_reviewer_fork_overlays pulls edits from reviewer fork files", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_forks_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a fork file for Darina
  wb_darina <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_darina, "Comments")
  fork_c <- data.frame(
    file = "eda.docx",
    comment_id = "10",
    darina = "TRUE",
    darina_comment = "Approved by Darina",
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb_darina, "Comments", fork_c)
  openxlsx::saveWorkbook(wb_darina, file.path(tmp_dir, "review_tracker_darina.xlsx"))

  existing_c <- tibble::tibble(
    file = "eda.docx",
    comment_id = "10",
    darina = NA_character_,
    darina_comment = NA_character_,
    jka = "TRUE"
  )
  existing_rl <- tibble::tibble()

  overlays <- apply_reviewer_fork_overlays(
    output_dir = tmp_dir,
    tracker_basename = "review_tracker.xlsx",
    existing_comments = existing_c,
    existing_redlines = existing_rl,
    fork_reviewers = c("jka", "darina", "zhao")
  )

  res_c <- overlays$comments
  expect_equal(res_c$darina[1], "TRUE")
  expect_equal(res_c$darina_comment[1], "Approved by Darina")
  expect_equal(res_c$jka[1], "TRUE")
})

# =============================================================================
# TESTS: END-TO-END WORKFLOW
# =============================================================================

test_that("cbe_docx_review_extract completes end-to-end and generates all artifacts", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_e2e_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create two docx files in input directory
  doc1 <- file.path(tmp_dir, "intro_MRW_11_21_25.docx")
  doc2 <- file.path(tmp_dir, "eda_11_22_25.docx")

  xml_c1 <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="1" w:author="MRW" w:date="2025-11-21T10:00:00Z">\n',
    '    <w:p><w:r><w:t>Recurring note</w:t></w:r></w:p>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )
  xml_d1 <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:commentRangeStart w:id="1"/>\n',
    '      <w:r><w:t>Introduction text</w:t></w:r>\n',
    '      <w:commentRangeEnd w:id="1"/>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )

  xml_c2 <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="1" w:author="MRW" w:date="2025-11-22T10:00:00Z">\n',
    '    <w:p><w:r><w:t>Recurring note</w:t></w:r></w:p>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )
  xml_d2 <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:commentRangeStart w:id="1"/>\n',
    '      <w:r><w:t>EDA text with tracked change </w:t></w:r>\n',
    '      <w:ins w:author="Editor" w:date="2025-11-22T11:00:00Z"><w:r><w:t>added</w:t></w:r></w:ins>\n',
    '      <w:commentRangeEnd w:id="1"/>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )

  create_synthetic_docx(doc1, xml_d1, xml_c1)
  create_synthetic_docx(doc2, xml_d2, xml_c2)

  out_dir <- file.path(tmp_dir, "output")

  res <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    tracker = "review_tracker.xlsx",
    fork_reviewers = c("jka", "darina"),
    verbose = FALSE
  )

  expect_s3_class(res, "cbe_review_extract")

  # Check master tracker created
  master_path <- file.path(out_dir, "review_tracker.xlsx")
  expect_true(file.exists(master_path))

  sheets <- openxlsx::getSheetNames(master_path)
  expect_true("Comments" %in% sheets)
  expect_true("SuggestedChanges" %in% sheets)
  expect_true("Documents" %in% sheets)
  expect_true("docXwalk" %in% sheets)
  expect_true("TrackedChanges" %in% sheets)
  expect_true("CommentSummary" %in% sheets)

  # Check reviewer fork workbooks created
  fork_jka <- file.path(out_dir, "review_tracker_jka.xlsx")
  fork_darina <- file.path(out_dir, "review_tracker_darina.xlsx")
  expect_true(file.exists(fork_jka))
  expect_true(file.exists(fork_darina))

  # In jka fork, check that darina columns were dropped
  jka_c_cols <- names(openxlsx::read.xlsx(fork_jka, sheet = "Comments"))
  expect_true("jka" %in% jka_c_cols)
  expect_false("darina" %in% jka_c_cols)
  expect_false("wolfson" %in% jka_c_cols)

  # Check CSV exports
  expect_true(file.exists(file.path(out_dir, "comments.csv")))
  expect_true(file.exists(file.path(out_dir, "tracked_changes.csv")))
  expect_true(file.exists(file.path(out_dir, "suggested_changes.csv")))

  # Check duplicate counts and recurring summary
  expect_equal(nrow(res$summary), 1)
  expect_equal(res$summary$comment_text[1], "Recurring note")
  expect_equal(res$summary$occurrences[1], 2L)

  # Print method executes cleanly
  expect_output(print(res), "REVIEW EXTRACTION & WORKFLOW TRACKER UPDATE COMPLETE")
})

test_that("extract_comment_locations handles multi-paragraph comment ranges", {
  tmp_dir <- tempfile(pattern = "test_multipara_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  docx_file <- file.path(tmp_dir, "multipara.docx")

  doc_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:commentRangeStart w:id="100"/>\n',
    '      <w:r><w:t>Start in para 1.</w:t></w:r>\n',
    '    </w:p>\n',
    '    <w:p>\n',
    '      <w:r><w:t>Middle para 2.</w:t></w:r>\n',
    '    </w:p>\n',
    '    <w:p>\n',
    '      <w:r><w:t>End in para 3.</w:t></w:r>\n',
    '      <w:commentRangeEnd w:id="100"/>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )

  comments_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="100" w:author="Reviewer" w:date="2025-11-20T00:00:00Z">\n',
    '    <w:p><w:r><w:t>Span across 3 paragraphs.</w:t></w:r></w:p>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )

  create_synthetic_docx(docx_file, doc_xml, comments_xml)

  res <- extract_from_docx(docx_file)
  expect_equal(nrow(res$comments), 1)
  c1 <- res$comments[1, ]
  expect_equal(c1$paragraph_number, 1L)
  expect_equal(c1$end_paragraph_number, 3L)
  expect_true(grepl("Start in para 1", c1$selected_text))
  expect_true(grepl("Middle para 2", c1$selected_text))
  expect_true(grepl("End in para 3", c1$selected_text))
  expect_true(grepl("Start in para 1", c1$paragraph_context))
  expect_true(grepl("End in para 3", c1$paragraph_context))
})

test_that("three reviewers signing off across separate forks achieve consensus in master", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_consensus_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  doc_file <- file.path(tmp_dir, "eda_11_21_25.docx")
  xml_c <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="1" w:author="MRW" w:date="2025-11-21T10:00:00Z">\n',
    '    <w:p><w:r><w:t>Check distribution outliers.</w:t></w:r></w:p>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )
  xml_d <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:commentRangeStart w:id="1"/>\n',
    '      <w:r><w:t>Outliers observed in tail.</w:t></w:r>\n',
    '      <w:commentRangeEnd w:id="1"/>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )
  create_synthetic_docx(doc_file, xml_d, xml_c)

  out_dir <- file.path(tmp_dir, "tracker_out")

  # Run 1: Initial extraction
  res1 <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    fork_reviewers = c("jka", "darina", "zhao")
  )

  # Reviewer 1 (JKA) signs off in review_tracker_jka.xlsx
  jka_fork <- file.path(out_dir, "review_tracker_jka.xlsx")
  wb_jka <- openxlsx::loadWorkbook(jka_fork)
  openxlsx::writeData(wb_jka, "Comments", "TRUE", startCol = which(names(openxlsx::read.xlsx(jka_fork, "Comments")) == "jka"), startRow = 2)
  openxlsx::saveWorkbook(wb_jka, jka_fork, overwrite = TRUE)

  # Reviewer 2 (Darina) signs off in review_tracker_darina.xlsx
  darina_fork <- file.path(out_dir, "review_tracker_darina.xlsx")
  wb_darina <- openxlsx::loadWorkbook(darina_fork)
  openxlsx::writeData(wb_darina, "Comments", "TRUE", startCol = which(names(openxlsx::read.xlsx(darina_fork, "Comments")) == "darina"), startRow = 2)
  openxlsx::saveWorkbook(wb_darina, darina_fork, overwrite = TRUE)

  # Reviewer 3 (Zhao) signs off in review_tracker_zhao.xlsx
  zhao_fork <- file.path(out_dir, "review_tracker_zhao.xlsx")
  wb_zhao <- openxlsx::loadWorkbook(zhao_fork)
  openxlsx::writeData(wb_zhao, "Comments", "TRUE", startCol = which(names(openxlsx::read.xlsx(zhao_fork, "Comments")) == "zhao"), startRow = 2)
  openxlsx::saveWorkbook(wb_zhao, zhao_fork, overwrite = TRUE)

  # Run 2: Re-run extraction, pulling all 3 forks back into master
  res2 <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    fork_reviewers = c("jka", "darina", "zhao")
  )

  c_row <- res2$comments[1, ]
  expect_equal(c_row$jka, "TRUE")
  expect_equal(c_row$darina, "TRUE")
  expect_equal(c_row$zhao, "TRUE")

  # In master tracker CSV, resolved is evaluated to TRUE
  csv_data <- utils::read.csv(res2$paths$comments_csv, stringsAsFactors = FALSE)
  expect_true(is_review_true(csv_data$resolved[1]))

  # Documents sheet also evaluates resolved
  expect_true(res2$documents$resolved[1])
})

# =============================================================================
# TESTS: CONFIGURABLE REVIEWERS (review_config)
# =============================================================================

test_that("review_config returns generic defaults and round-trips custom values", {
  old_fork <- getOption("review.fork_reviewers")
  old_signoff <- getOption("review.signoff_reviewers")
  old_mode <- getOption("review.documents_sheet_mode")
  on.exit(options(
    review.fork_reviewers = old_fork,
    review.signoff_reviewers = old_signoff,
    review.documents_sheet_mode = old_mode
  ), add = TRUE)

  options(review.fork_reviewers = NULL, review.signoff_reviewers = NULL, review.documents_sheet_mode = NULL)
  defaults <- review_config()
  expect_equal(defaults$fork_reviewers, c("reviewer_1", "reviewer_2", "reviewer_3"))
  expect_equal(defaults$signoff_reviewers, character(0))
  expect_equal(defaults$documents_sheet_mode, "columns")

  cfg <- review_config(
    fork_reviewers = c("alpha", "beta"),
    signoff_reviewers = "lead_pi",
    documents_sheet_mode = "per_reviewer"
  )
  expect_equal(cfg$fork_reviewers, c("alpha", "beta"))
  expect_equal(cfg$signoff_reviewers, "lead_pi")
  expect_equal(cfg$documents_sheet_mode, "per_reviewer")

  # Persisted via options() until reset
  expect_equal(review_config()$fork_reviewers, c("alpha", "beta"))

  expect_error(review_config(documents_sheet_mode = "not_a_mode"))
})

test_that("canonical_columns and redline_canonical_columns scale to an arbitrary reviewer count", {
  # Zero reviewers
  cc0 <- canonical_columns(fork_reviewers = character(0), signoff_reviewers = character(0))
  expect_true(all(c("file", "resolved", "doc_status") %in% cc0))
  expect_false(any(grepl("^reviewer_", cc0)))

  # Five fork reviewers + two sign-off reviewers, arbitrary names
  fr <- paste0("rev_", 1:5)
  sr <- c("lead_pi", "co_pi")
  cc <- canonical_columns(fork_reviewers = fr, signoff_reviewers = sr)
  expect_true(all(c(fr, paste0(fr, "_comment"), sr, paste0(sr, "_comment")) %in% cc))

  rcc <- redline_canonical_columns(fork_reviewers = fr, signoff_reviewers = sr)
  expect_true("is_comment" %in% rcc)
  expect_true(all(c(fr, sr) %in% rcc))
})

test_that("reviewer_fork_drop_columns hides sign-off and other fork reviewers", {
  drop <- reviewer_fork_drop_columns("rev_1", fork_reviewers = paste0("rev_", 1:3), signoff_reviewers = "lead_pi")
  expect_false("rev_1" %in% drop)
  expect_true(all(c("rev_2", "rev_3", "lead_pi", "lead_pi_comment") %in% drop))
})

test_that("extract_docx_stem recognizes a configured reviewer id longer than generic initials", {
  expect_equal(extract_docx_stem("eda_longreviewerid_11_21_25.docx", reviewer_ids = "longreviewerid"), "eda")
  # Unconfigured long ids are left alone
  expect_equal(extract_docx_stem("eda_longreviewerid_11_21_25.docx", reviewer_ids = character(0)), "eda_longreviewerid")
})

test_that("cbe_docx_review_extract works end-to-end with arbitrarily named, non-default reviewers", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_custom_reviewers_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  doc_file <- file.path(tmp_dir, "eda_11_21_25.docx")
  xml_c <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:comment w:id="1" w:author="Reviewer" w:date="2025-11-21T10:00:00Z">\n',
    '    <w:p><w:r><w:t>Please double-check this.</w:t></w:r></w:p>\n',
    '  </w:comment>\n',
    '</w:comments>'
  )
  xml_d <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">\n',
    '  <w:body>\n',
    '    <w:p>\n',
    '      <w:commentRangeStart w:id="1"/>\n',
    '      <w:r><w:t>Flagged text.</w:t></w:r>\n',
    '      <w:commentRangeEnd w:id="1"/>\n',
    '    </w:p>\n',
    '  </w:body>\n',
    '</w:document>'
  )
  create_synthetic_docx(doc_file, xml_d, xml_c)

  out_dir <- file.path(tmp_dir, "output")
  fork_reviewers <- c("alpha", "beta", "gamma", "delta")
  signoff_reviewers <- "lead_pi"

  res <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers,
    verbose = FALSE
  )

  expect_s3_class(res, "cbe_review_extract")
  expect_true(all(fork_reviewers %in% names(res$comments)))
  expect_true(all(fork_reviewers %in% names(res$documents)))
  expect_true(signoff_reviewers %in% names(res$documents))
  expect_true(paste0(signoff_reviewers, "_comment") %in% names(res$documents))

  master_path <- file.path(out_dir, "review_tracker.xlsx")
  doc_headers <- names(openxlsx::read.xlsx(master_path, sheet = "Documents"))
  expect_equal(
    doc_headers,
    c(
      "file", "pipeline_stage", "comment_count", "resolved_comment_count",
      "reply_count", "tracked_change_count",
      fork_reviewers, "resolved", signoff_reviewers, paste0(signoff_reviewers, "_comment")
    )
  )

  # Each fork reviewer gets a fork workbook exposing only their own column
  for (rev in fork_reviewers) {
    fork_path <- file.path(out_dir, sprintf("review_tracker_%s.xlsx", rev))
    expect_true(file.exists(fork_path))
    fork_cols <- names(openxlsx::read.xlsx(fork_path, sheet = "Comments"))
    expect_true(rev %in% fork_cols)
    other_forks <- setdiff(fork_reviewers, rev)
    expect_false(any(other_forks %in% fork_cols))
    expect_false(signoff_reviewers %in% fork_cols)
  }

  # No fork workbook is generated for the sign-off reviewer
  expect_false(file.exists(file.path(out_dir, sprintf("review_tracker_%s.xlsx", signoff_reviewers))))
})

test_that("cbe_docx_review_extract supports zero fork reviewers with only a sign-off reviewer", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_signoff_only_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  doc_file <- file.path(tmp_dir, "eda_11_21_25.docx")
  create_synthetic_docx(doc_file)

  out_dir <- file.path(tmp_dir, "output")
  res <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    fork_reviewers = character(0),
    signoff_reviewers = "lead_pi",
    verbose = FALSE
  )

  expect_s3_class(res, "cbe_review_extract")
  expect_equal(length(res$paths$forks), 0)
  expect_false(all(res$documents$resolved))

  master_path <- file.path(out_dir, "review_tracker.xlsx")
  doc_headers <- names(openxlsx::read.xlsx(master_path, sheet = "Documents"))
  expect_true("lead_pi" %in% doc_headers)
  expect_true("resolved" %in% doc_headers)
})

test_that("documents_sheet_mode = 'per_reviewer' writes one Documents_<id> sheet per reviewer", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not available")

  tmp_dir <- tempfile(pattern = "test_per_reviewer_sheets_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  doc_file <- file.path(tmp_dir, "eda_11_21_25.docx")
  create_synthetic_docx(doc_file)

  out_dir <- file.path(tmp_dir, "output")
  fork_reviewers <- c("alpha", "beta")
  signoff_reviewers <- "lead_pi"

  res <- cbe_docx_review_extract(
    input_dir = tmp_dir,
    output_dir = out_dir,
    fork_reviewers = fork_reviewers,
    signoff_reviewers = signoff_reviewers,
    documents_sheet_mode = "per_reviewer",
    verbose = FALSE
  )

  expect_s3_class(res, "cbe_review_extract")

  master_path <- file.path(out_dir, "review_tracker.xlsx")
  sheets <- openxlsx::getSheetNames(master_path)
  for (rev in c(fork_reviewers, signoff_reviewers)) {
    expect_true(sprintf("Documents_%s", rev) %in% sheets)
  }

  doc_headers <- names(openxlsx::read.xlsx(master_path, sheet = "Documents"))
  expect_false(any(c(fork_reviewers, signoff_reviewers) %in% doc_headers))
  expect_true("resolved" %in% doc_headers)

  alpha_headers <- names(openxlsx::read.xlsx(master_path, sheet = "Documents_alpha"))
  expect_equal(alpha_headers, c("file", "pipeline_stage", "alpha"))

  lead_pi_headers <- names(openxlsx::read.xlsx(master_path, sheet = "Documents_lead_pi"))
  expect_equal(lead_pi_headers, c("file", "pipeline_stage", "lead_pi", "lead_pi_comment"))
})

