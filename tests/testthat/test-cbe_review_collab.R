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

test_that("a row a reviewer adds to their fork becomes a General Comment with a neutral id (redacted forks)", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(
    comment_text = "Overall: please rerun all tables with the final cut.", rev_a_comment = "applies to the whole report"
  ))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)

  expect_equal(nrow(r2$comments), 3L)
  gen <- r2$comments[r2$comments$comment_id == "gen_1", ]
  expect_equal(nrow(gen), 1L)
  expect_equal(gen$doc_status, "General Comment")
  expect_equal(gen$file, "(general)")
  expect_equal(gen$comment_text, "Overall: please rerun all tables with the final cut.")
  expect_equal(gen$rev_a_comment, "applies to the whole report")
  expect_equal(sum(r2$comments$doc_status == "Active"), 2L)

  # no id, column or file anywhere in the other reviewer's fork says who wrote it
  fork_b <- a6_sheet(r2$paths$forks[["rev_b"]], "Comments")
  expect_true("gen_1" %in% fork_b$comment_id)
  expect_false(any(grepl("rev_a", unlist(fork_b[fork_b$comment_id == "gen_1", ]), fixed = TRUE)))
  expect_false(any(c("rev_a", "rev_a_comment") %in% names(fork_b)))

  # it survives further rounds unchanged, once
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(sum(r3$comments$comment_id == "gen_1"), 1L)
  expect_equal(nrow(r3$comments), 3L)
  expect_equal(r3$comments$doc_status[r3$comments$comment_id == "gen_1"], "General Comment")
})

test_that("in collaborative mode the id names the reviewer", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out, fork_mode = "collaborative")
  collab_add_row(r1$paths$forks[["rev_a"]], list(comment_text = "A: remark"))
  collab_add_row(r1$paths$forks[["rev_b"]], list(comment_text = "B: remark"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out, fork_mode = "collaborative")
  expect_setequal(r2$comments$comment_id[r2$comments$doc_status == "General Comment"], c("gen_rev_a_1", "gen_rev_b_1"))
})

test_that("general comments are numbered across reviewers and blank added rows are ignored", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(comment_text = "A: first general remark"))
  collab_add_row(r1$paths$forks[["rev_b"]], list(comment_text = "B: a remark", rev_b = "TRUE"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_setequal(r2$comments$comment_id[r2$comments$doc_status == "General Comment"], c("gen_1", "gen_2"))

  collab_add_row(r2$paths$forks[["rev_a"]], list(comment_text = "A: second general remark"))
  collab_add_row(r2$paths$forks[["rev_a"]], list(file = "   ")) # nothing a reviewer wrote
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_setequal(r3$comments$comment_id[r3$comments$doc_status == "General Comment"], c("gen_1", "gen_2", "gen_3"))
})

test_that("a text-only row that names a real document is kept, under that document", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(file = "EDA.docx ", comment_text = "A remark on the whole of eda"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  gen <- r2$comments[r2$comments$doc_status == "General Comment", ]
  expect_equal(nrow(gen), 1L)
  expect_equal(gen$file, "eda.docx") # spelled as the master spells it
  # a file that is not a document of the tracker becomes (general), not a phantom document
  collab_add_row(r2$paths$forks[["rev_a"]], list(file = "typo.docx", comment_text = "Another remark"))
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(r3$comments$file[r3$comments$comment_text == "Another remark"], "(general)")
  expect_false("typo.docx" %in% r3$documents$file)
})

test_that("a general comment is kept when the documents have no Word comments", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out <- file.path(d, "out")
  a6_docx(file.path(d, "plain.docx"), a6_para("Only a tracked change.", ins = "added words"))
  r1 <- docx_review_extract(d, output_dir = out)
  expect_equal(nrow(r1$comments), 0L)
  collab_add_row(r1$paths$forks[["rev_a"]], list(comment_text = "Overall remark on a document with no comments"))
  r2 <- docx_review_extract(d, output_dir = out)
  expect_equal(nrow(r2$comments), 1L)
  expect_equal(r2$comments$doc_status, "General Comment")
})

test_that("sending the same fork back twice does not duplicate a general comment", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  fork <- r1$paths$forks[["rev_a"]]
  keep <- file.path(dirname(fork), "first_round_fork.xlsx")
  file.copy(fork, keep)
  collab_add_row(fork, list(comment_text = "A general remark", rev_a_comment = "n1"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(sum(r2$comments$doc_status == "General Comment"), 1L)
  # the reviewer sends the earlier version of the fork again, with the same row added
  file.copy(keep, fork, overwrite = TRUE)
  collab_add_row(fork, list(comment_text = "A  general remark", rev_a_comment = "n1"))
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(sum(r3$comments$doc_status == "General Comment"), 1L)
})

test_that("changing the file of one's own general comment in the fork does not add a copy", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  collab_add_row(r1$paths$forks[["rev_a"]], list(comment_text = "A general remark"))
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  a6_set_cells(r2$paths$forks[["rev_a"]], "Comments", "comment_text", "A general remark", list(file = "eda.docx"))
  r3 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(sum(r3$comments$doc_status == "General Comment"), 1L)
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
  # several manifests: ambiguous, so a warning names all of them and the newest is used
  expect_warning(
    got <- find_pipeline_manifest(base, analysis_dir = character(0), discover = TRUE),
    "Several pipeline manifests were found.*Name the one you want with manifest_path"
  )
  expect_equal(normalizePath(got, winslash = "/"), normalizePath(newer, winslash = "/"))

  # exactly one: used, and said
  unlink(file.path(base, "analysis", "run2"), recursive = TRUE)
  expect_message(
    got1 <- find_pipeline_manifest(base, analysis_dir = character(0), discover = TRUE),
    "Using the pipeline manifest"
  )
  expect_equal(normalizePath(got1, winslash = "/"), normalizePath(older, winslash = "/"))

  # a manifest next to the reviewed folder's parent is found from a folder inside it
  inner <- file.path(base, "reviews", "round1")
  dir.create(inner, recursive = TRUE)
  top <- file.path(base, "reports_to_render.xlsx")
  file.create(top)
  expect_warning(
    got_top <- find_pipeline_manifest(inner, analysis_dir = character(0), discover = TRUE),
    "Several pipeline manifests"
  )
  expect_equal(normalizePath(got_top, winslash = "/"), normalizePath(top, winslash = "/"))
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

test_that("manifest discovery works in a folder whose name holds glob characters", {
  parent <- withr::local_tempdir(pattern = "manifest_")
  base <- file.path(parent, "Study [A]")
  dir.create(file.path(base, "analysis", "p"), recursive = TRUE)
  target <- file.path(base, "analysis", "p", "reports_to_render.xlsx")
  file.create(target)
  withr::local_envvar(c(TEMPLECBE_ANALYSIS_ROOT = NA))
  withr::local_dir(parent)
  old <- review_options_reset()
  withr::defer(options(old))
  expect_message(got <- find_pipeline_manifest(base, analysis_dir = character(0), discover = TRUE))
  expect_equal(normalizePath(got, winslash = "/"), normalizePath(target, winslash = "/"))
})

test_that("the result records which manifest was used", {
  s <- collab_setup()
  old <- review_options_reset()
  withr::defer(options(old))
  a6_options()
  r <- docx_review_extract(s$dir, output_dir = s$out)
  expect_true(is.na(r$paths$manifest))
})

# ---- general rows in SuggestedChanges ------------------------------------------------------------

test_that("a row a reviewer adds to the SuggestedChanges sheet of their fork is kept as a General Comment", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out <- file.path(d, "out")
  a6_docx(file.path(d, "eda.docx"), paste0(a6_commented_para(0, "alpha text"), a6_para("Gamma.", ins = "added gamma")),
          comments = a6_comment(0, "first comment"))
  r1 <- docx_review_extract(d, output_dir = out)
  expect_equal(nrow(r1$redlines), 1L)
  collab_add_row(r1$paths$forks[["rev_a"]], list(original_text = "Rewrite the abstract as a whole", rev_a_comment = "overall"),
                 sheet = "SuggestedChanges")
  r2 <- docx_review_extract(d, output_dir = out)
  expect_equal(nrow(r2$redlines), 2L)
  gen <- r2$redlines[r2$redlines$doc_status == "General Comment", ]
  expect_equal(nrow(gen), 1L)
  expect_equal(gen$original_text, "Rewrite the abstract as a whole")
  expect_equal(gen$file, "(general)")
  expect_equal(gen$rev_a_comment, "overall")
  # kept, once, in later rounds
  r3 <- docx_review_extract(d, output_dir = out)
  expect_equal(sum(r3$redlines$doc_status == "General Comment"), 1L)
  expect_equal(nrow(r3$redlines), 2L)
})

test_that("a general suggested change is kept when the documents have no tracked changes", {
  s <- collab_setup()
  r1 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(nrow(r1$redlines), 0L)
  collab_add_row(r1$paths$forks[["rev_a"]], list(original_text = "Check every table title"), sheet = "SuggestedChanges")
  r2 <- docx_review_extract(s$dir, output_dir = s$out)
  expect_equal(nrow(r2$redlines), 1L)
  expect_equal(r2$redlines$doc_status, "General Comment")
})

test_that("a resent or re-filed general suggested change is not added twice, and a text-only one with a real file is kept", {
  skip_if_not_installed("openxlsx")
  a6_options()
  d <- a6_dir()
  out <- file.path(d, "out")
  a6_docx(file.path(d, "eda.docx"), paste0(a6_commented_para(0, "alpha text"), a6_para("Gamma.", ins = "added gamma")),
          comments = a6_comment(0, "first comment"))
  r1 <- docx_review_extract(d, output_dir = out)
  fork <- r1$paths$forks[["rev_a"]]
  keep <- file.path(d, "round1_fork.xlsx")
  file.copy(fork, keep)
  collab_add_row(fork, list(original_text = "Rewrite the abstract", rev_a_comment = "n1"), sheet = "SuggestedChanges")
  r2 <- docx_review_extract(d, output_dir = out)
  expect_equal(sum(r2$redlines$doc_status == "General Comment"), 1L)
  # the first-round fork is sent again with the same row added (file blank, the master says "(general)")
  file.copy(keep, fork, overwrite = TRUE)
  collab_add_row(fork, list(original_text = "Rewrite  the abstract", rev_a_comment = "n1"), sheet = "SuggestedChanges")
  r3 <- docx_review_extract(d, output_dir = out)
  expect_equal(sum(r3$redlines$doc_status == "General Comment"), 1L)

  # a row with a real file name and only text, no sign-off or note, survives the rounds that follow
  collab_add_row(r3$paths$forks[["rev_a"]], list(file = "eda.docx", original_text = "Reword the conclusion"), sheet = "SuggestedChanges")
  r4 <- docx_review_extract(d, output_dir = out)
  expect_true("Reword the conclusion" %in% r4$redlines$original_text)
  r5 <- docx_review_extract(d, output_dir = out)
  expect_true("Reword the conclusion" %in% r5$redlines$original_text)
  expect_equal(sum(r5$redlines$doc_status == "General Comment"), 2L)
})
