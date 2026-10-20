# Collaborative forks, general comments added in a fork, and opt-in manifest discovery
# for docx_review_extract(). Everything is invented and lives in temp folders.

collab_setup <- function(env = parent.frame(), fork_mode = NULL, signoff = character(0)) {
  skip_if_not_installed("openxlsx")
  a6_options(signoff = signoff, env = env)
  d <- a6_dir(env)
  a6_docx(
    file.path(d, "eda.docx"),
    paste0(a6_commented_para(0, "alpha text"), a6_commented_para(1, "beta text")),
    comments = c(a6_comment(0, "first comment"), a6_comment(1, "second comment"))
  )
  list(dir = d, out = file.path(d, "out"), fork_mode = fork_mode)
}

# Append a row to the Comments sheet of a fork workbook
collab_add_row <- function(xlsx, values, sheet = "Comments") {
  wb <- openxlsx::loadWorkbook(xlsx)
  df <- openxlsx::readWorkbook(wb, sheet, colNames = TRUE)
  row_i <- nrow(df) + 2L
  for (nm in names(values)) {
    openxlsx::writeData(wb, sheet, values[[nm]], startCol = which(names(df) == nm), startRow = row_i)
  }
  openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)
}

# ---- fork_mode -----------------------------------------------------------------------------------

test_that("review_config() takes fork_mode, validates it and resets it", {
  old <- options(review.fork_mode = NULL)
  withr::defer(options(old))
  expect_identical(review_config()$fork_mode, "redacted")
  expect_identical(review_config(fork_mode = "collaborative")$fork_mode, "collaborative")
  expect_error(review_config(fork_mode = "open"), "should be one of")
  expect_identical(review_config()$fork_mode, "collaborative") # a rejected call changes nothing
  expect_identical(review_config(reset = TRUE)$fork_mode, "redacted")
})

test_that("a redacted fork (the default) holds only its own reviewer's sign-off columns", {
  s <- collab_setup(signoff = "lead")
  r <- docx_review_extract(s$dir, output_dir = s$out)
  fork_a <- a6_sheet(r$paths$forks[["rev_a"]], "Comments")
  expect_true(all(c("rev_a", "rev_a_comment") %in% names(fork_a)))
  expect_false(any(c("rev_b", "rev_b_comment", "lead", "lead_comment", "author") %in% names(fork_a)))
})

test_that("a collaborative fork also shows the other fork reviewers, never the sign-off reviewers or authors", {
  s <- collab_setup(signoff = "lead")
  r <- docx_review_extract(s$dir, output_dir = s$out, fork_mode = "collaborative")
  for (rev in c("rev_a", "rev_b")) {
    cm <- a6_sheet(r$paths$forks[[rev]], "Comments")
    expect_true(all(c("rev_a", "rev_a_comment", "rev_b", "rev_b_comment") %in% names(cm)), info = rev)
    expect_false(any(c("lead", "lead_comment", "author") %in% names(cm)), info = rev)
    docs <- a6_sheet(r$paths$forks[[rev]], "Documents")
    expect_true(all(c("rev_a", "rev_b") %in% names(docs)), info = rev)
    expect_false("lead" %in% names(docs), info = rev)
  }
})

test_that("fork_mode can come from review_config()", {
  s <- collab_setup()
  old <- options(review.fork_mode = "collaborative")
  withr::defer(options(old))
  r <- docx_review_extract(s$dir, output_dir = s$out)
  expect_true("rev_b" %in% names(a6_sheet(r$paths$forks[["rev_a"]], "Comments")))
})

test_that("collaborative mode shows a colleague's notes next round but merges back only a reviewer's own columns", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out, fork_mode = "collaborative")
  a6_set_cells(r1$paths$forks[["rev_b"]], "Comments", "comment_text", "first comment",
               list(rev_b = "TRUE", rev_b_comment = "agree with the wording"))
  # rev_a also types into rev_b's columns in their own fork: this must not reach the master
  a6_set_cells(r1$paths$forks[["rev_a"]], "Comments", "comment_text", "first comment",
               list(rev_a = "TRUE", rev_b = "TRUE", rev_b_comment = "forged by rev_a"))

  r2 <- docx_review_extract(s$dir, output_dir = s$out, fork_mode = "collaborative")
  first <- r2$comments[r2$comments$comment_text == "first comment", ]
  expect_equal(first$rev_a, "TRUE")
  expect_equal(first$rev_b_comment, "agree with the wording")
  # and the regenerated fork of rev_a now shows rev_b's note
  fork_a <- a6_sheet(r2$paths$forks[["rev_a"]], "Comments")
  expect_equal(fork_a$rev_b_comment[fork_a$comment_text == "first comment"], "agree with the wording")
})

# ---- general comments added in a fork ------------------------------------------------------------

test_that("a row a reviewer adds to their fork becomes a General Comment with a generated id", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(
    comment_text = "Overall: please rerun all tables with the final cut.", rev_a_comment = "applies to the whole report"
  ))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)

  expect_equal(nrow(r2$comments), 3L)
  gen <- r2$comments[r2$comments$comment_id == "gen_rev_a_1", ]
  expect_equal(nrow(gen), 1L)
  expect_equal(gen$doc_status, "General Comment")
  expect_equal(gen$comment_text, "Overall: please rerun all tables with the final cut.")
  expect_equal(gen$rev_a_comment, "applies to the whole report")
  # real comments are untouched
  expect_equal(sum(r2$comments$doc_status == "Active"), 2L)

  # it is in the master workbook and in every fork, and survives further rounds unchanged
  master <- a6_sheet(r2$paths$master, "Comments")
  expect_true("gen_rev_a_1" %in% master$comment_id)
  expect_true("gen_rev_a_1" %in% a6_sheet(r2$paths$forks[["rev_b"]], "Comments")$comment_id)
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(sum(r3$comments$comment_id == "gen_rev_a_1"), 1L)
  expect_equal(nrow(r3$comments), 3L)
  expect_equal(r3$comments$doc_status[r3$comments$comment_id == "gen_rev_a_1"], "General Comment")
})

test_that("general comments are numbered per reviewer and blank added rows are ignored", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(comment_text = "A: first general remark"))
  collab_add_row(r1$paths$forks[["rev_b"]], list(comment_text = "B: a remark", rev_b = "TRUE"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_setequal(r2$comments$comment_id[r2$comments$doc_status == "General Comment"], c("gen_rev_a_1", "gen_rev_b_1"))

  collab_add_row(r2$paths$forks[["rev_a"]], list(comment_text = "A: second general remark"))
  collab_add_row(r2$paths$forks[["rev_a"]], list(file = "   ")) # nothing a reviewer wrote
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_setequal(
    r3$comments$comment_id[r3$comments$doc_status == "General Comment"],
    c("gen_rev_a_1", "gen_rev_a_2", "gen_rev_b_1")
  )
})

test_that("a comment the master dropped is not brought back as a General Comment", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  # the reviewer's fork still lists "second comment"; the master loses the row by hand
  wb <- openxlsx::loadWorkbook(r1$paths$master)
  df <- openxlsx::readWorkbook(wb, "Comments", colNames = TRUE)
  drop <- which(df$comment_text == "second comment") + 1L
  openxlsx::deleteData(wb, "Comments", cols = seq_len(ncol(df)), rows = drop, gridExpand = TRUE)
  openxlsx::saveWorkbook(wb, r1$paths$master, overwrite = TRUE)
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_false(any(r2$comments$doc_status == "General Comment", na.rm = TRUE))
})

# ---- manifest discovery --------------------------------------------------------------------------

test_that("manifest discovery is opt-in: without it nothing is searched, with it the usual places are", {
  base <- withr::local_tempdir(pattern = "manifest_")
  dir.create(file.path(base, "analysis", "run1"), recursive = TRUE)
  dir.create(file.path(base, "analysis", "run2"), recursive = TRUE)
  older <- file.path(base, "analysis", "run1", "reports_to_render.xlsx")
  newer <- file.path(base, "analysis", "run2", "reports_to_render.xlsx")
  file.create(c(older, newer))
  Sys.setFileTime(older, as.POSIXct("2024-01-01", tz = "UTC"))
  Sys.setFileTime(newer, as.POSIXct("2025-01-01", tz = "UTC"))
  withr::local_dir(base)
  withr::local_envvar(c(TEMPLECBE_ANALYSIS_ROOT = NA))
  old <- review_options_reset()
  withr::defer(options(old))

  expect_null(find_pipeline_manifest(base, analysis_dir = character(0)))
  msgs <- testthat::capture_messages(
    got <- find_pipeline_manifest(base, analysis_dir = character(0), discover = TRUE)
  )
  expect_equal(normalizePath(got, winslash = "/"), normalizePath(newer, winslash = "/"))
  expect_match(paste(msgs, collapse = ""), "Several pipeline manifests were found")

  # a manifest next to the reviewed folder is found from a folder inside it
  inner <- file.path(base, "reviews", "round1")
  dir.create(inner, recursive = TRUE)
  top <- file.path(base, "reports_to_render.xlsx")
  file.create(top)
  expect_equal(
    normalizePath(find_pipeline_manifest(inner, analysis_dir = character(0), discover = TRUE), winslash = "/"),
    normalizePath(top, winslash = "/")
  )
  # a configured analysis_dir still wins over discovery
  expect_equal(
    normalizePath(find_pipeline_manifest(inner, analysis_dir = "analysis/run1", discover = TRUE), winslash = "/"),
    normalizePath(older, winslash = "/")
  )
  # nothing there: NULL
  empty <- withr::local_tempdir(pattern = "nomanifest_")
  withr::local_dir(empty)
  expect_null(find_pipeline_manifest(empty, analysis_dir = character(0), discover = TRUE))
})
