# Privacy and hardening tests for cbe_docx_review_extract() (audit chunk A6).
# All reviewer ids, stage names and texts below are invented.

# =============================================================================
# A6-16: REVIEWER IDS ARE VALIDATED AND NEVER TREATED AS REGULAR EXPRESSIONS
# =============================================================================

test_that("review_config() rejects reviewer ids that cannot be used in names, paths or patterns (A6-16)", {
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  unusable <- c(
    "c++", "j.doe", "a/b", "..", "", "has space", "dash-ed",
    "file", "Author", "resolved", "doc_status", "comment_count",
    "rev_comment", strrep("x", 21)
  )
  for (id in unusable) {
    expect_error(review_config(fork_reviewers = id), "Invalid reviewer id", info = sprintf("fork id '%s'", id))
    expect_error(review_config(signoff_reviewers = id), "Invalid reviewer id", info = sprintf("sign-off id '%s'", id))
  }
  expect_error(review_config(fork_reviewers = c("rev_a", "REV_A")), "more than once")
  expect_error(review_config(fork_reviewers = "rev_a", signoff_reviewers = "Rev_A"), "both a fork and a sign-off")
  expect_error(review_config(fork_reviewers = NA_character_), "without missing")
  expect_error(review_config(fork_reviewers = 1:2), "character vector")

  # A rejected call changes nothing, even for the argument that was valid
  review_config(fork_reviewers = c("rev_a", "rev_b"))
  expect_error(review_config(fork_reviewers = "rev_c", signoff_reviewers = "c++"), "Invalid reviewer id")
  expect_equal(review_config()$fork_reviewers, c("rev_a", "rev_b"))
  expect_equal(review_config()$signoff_reviewers, character(0))

  # Plain tokens up to 20 characters, in any case, are fine; so is "no fork reviewer"
  ok <- c(strrep("x", 20), "Rev_2", "_a", "0")
  expect_equal(review_config(fork_reviewers = ok)$fork_reviewers, ok)
  expect_equal(review_config(fork_reviewers = character(0))$fork_reviewers, character(0))
})

test_that("cbe_docx_review_extract() validates reviewer ids passed as arguments before writing anything (A6-16)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  input <- tempfile(pattern = "a6_ids_")
  dir.create(input)
  on.exit(unlink(input, recursive = TRUE), add = TRUE)
  build_review_docx(file.path(input, "doc_one.docx"))

  expect_error(cbe_docx_review_extract(input, fork_reviewers = c("rev_a", "../evil")), "Invalid reviewer id")
  expect_error(cbe_docx_review_extract(input, signoff_reviewers = "c++"), "Invalid reviewer id")
  expect_error(
    cbe_docx_review_extract(input, fork_reviewers = "a_long_reviewer_identifier_abc", documents_sheet_mode = "per_reviewer"),
    "longer than 20"
  )
  # Rejected before the output folder, a backup or a workbook is created
  expect_false(dir.exists(file.path(input, "review_extract")))

  # The longest allowed id still fits the 31-character sheet-name limit in per_reviewer mode
  longest <- strrep("r", 20)
  res <- cbe_docx_review_extract(
    input,
    fork_reviewers = longest, signoff_reviewers = "lead_x",
    documents_sheet_mode = "per_reviewer"
  )
  expect_true(paste0("Documents_", longest) %in% openxlsx::getSheetNames(res$paths$master))
  expect_true(file.exists(res$paths$forks[[longest]]))
})

test_that("extract_docx_stem() treats reviewer ids as literal text (A6-16)", {
  # '+' is a regex quantifier: an unescaped "c++" made every call fail
  expect_equal(extract_docx_stem("intro_c++.docx", reviewer_ids = c("j.doe", "c++")), "intro")
  # '.' is a wildcard: "j.doe" must not also strip "jXdoe"
  expect_equal(extract_docx_stem("intro_jXdoe.docx", reviewer_ids = "j.doe"), "intro_jxdoe")
  expect_equal(extract_docx_stem("intro_longreviewerid.docx", reviewer_ids = "longreviewerid"), "intro")

  # Ids from the configuration are used the same way
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)
  options(review.fork_reviewers = c("j.doe", "c++"))
  expect_equal(extract_docx_stem("intro_c++.docx"), "intro")
  expect_equal(extract_docx_stem("intro_jXdoe.docx"), "intro_jxdoe")

  special <- "a.b+c(d)[e]{f}^$|?*\\g"
  expect_true(grepl(paste0("^", regex_escape(special), "$"), special))
  expect_false(grepl(paste0("^", regex_escape("a.b"), "$"), "aXb"))
})

# =============================================================================
# A6-02 / A6-24: A FORK WORKBOOK HOLDS NOTHING OF THE OTHER REVIEWERS
# =============================================================================

# One document with a comment (Word author "Word Author One") and a tracked
# insertion (Word author "Word Author Two"), reviewed by two fork reviewers and
# one sign-off reviewer.
fork_fixture <- function(env = parent.frame()) {
  input <- withr::local_tempdir(pattern = "a6_fork_", .local_envir = env)
  build_review_docx(
    file.path(input, "doc_one.docx"),
    paragraphs = c("Alpha text.", "Beta text."),
    comments = list(list(id = 0, author = "Word Author One", text = "please check this", para = 1)),
    insertions = list(list(author = "Word Author Two", text = "inserted words", para = 2))
  )
  list(
    input = input,
    out = file.path(input, "out"),
    fork = c("rev_a", "rev_b"),
    signoff = "chair_x",
    note = "PRIVATE-NOTE-7731"
  )
}

test_that("the sign-off reviewer's private note and the other fork reviewers never reach a fork (A6-02)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  fx <- fork_fixture()
  run <- function() {
    cbe_docx_review_extract(fx$input, fx$out, fork_reviewers = fx$fork, signoff_reviewers = fx$signoff)
  }
  res <- run()

  # The sign-off reviewer records a status and a private note on the master's Documents sheet
  wb <- openxlsx::loadWorkbook(res$paths$master)
  at <- which(names(openxlsx::readWorkbook(wb, "Documents")) == fx$signoff)
  openxlsx::writeData(wb, "Documents", "TRUE", startCol = at, startRow = 2)
  openxlsx::writeData(wb, "Documents", fx$note, startCol = at + 1L, startRow = 2)
  openxlsx::saveWorkbook(wb, res$paths$master, overwrite = TRUE)

  res <- run()
  # The master keeps it ...
  master_text <- paste(unlist(xlsx_raw_parts(res$paths$master)), collapse = "\n")
  expect_true(grepl(fx$note, master_text, fixed = TRUE))

  # ... and the fork sent to rev_a does not: not in any cell, formula, header,
  # hidden column, hidden sheet or sheet name
  fork <- res$paths$forks[["rev_a"]]
  fork_text <- paste(unlist(xlsx_raw_parts(fork)), collapse = "\n")
  for (secret in c(fx$note, fx$signoff, "rev_b", "Word Author One", "Word Author Two")) {
    expect_false(grepl(secret, fork_text, fixed = TRUE), info = sprintf("'%s' found in the rev_a fork", secret))
  }
  expect_true(grepl("rev_a", fork_text, fixed = TRUE))

  docs <- readxl::read_excel(fork, sheet = "Documents", col_types = "text")
  expect_equal(
    names(docs),
    c(
      "file", "pipeline_stage", "comment_count", "resolved_comment_count",
      "reply_count", "tracked_change_count", "rev_a", "resolved"
    )
  )
  expect_equal(docs$comment_count, "1")
})

test_that("a fork's Documents sheet is filtered the same way in both documents_sheet_modes (A6-02)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  for (mode in c("columns", "per_reviewer")) {
    fx <- fork_fixture()
    res <- cbe_docx_review_extract(
      fx$input, fx$out,
      fork_reviewers = fx$fork, signoff_reviewers = fx$signoff, documents_sheet_mode = mode
    )

    # Write the master and the rev_a fork directly from a stored sign-off
    # (the writer is what filters; reading the note back is the merge's job)
    fr <- fx$fork
    so <- fx$signoff
    docs <- list(doc_one.docx = list(file = "doc_one.docx", chair_x = "TRUE", chair_x_comment = fx$note))
    write_to <- function(path, view) {
      write_review_tracker_excel(
        columns = canonical_columns(fr, so), comments_rows = res$comments, revisions_rows = res$revisions,
        processed_docs = file.path(fx$input, "doc_one.docx"), errors = tibble::tibble(),
        output_path = path, catalog = list(),
        redline_columns = redline_canonical_columns(fr, so), redlines_rows = res$redlines,
        existing_docs = docs, fork_reviewers = fr, signoff_reviewers = so,
        documents_sheet_mode = mode, reviewer_view = view
      )
      path
    }
    master <- write_to(tempfile(fileext = ".xlsx"), NULL)
    fork <- write_to(tempfile(fileext = ".xlsx"), "rev_a")
    withr::defer(unlink(c(master, fork)))

    master_text <- paste(unlist(xlsx_raw_parts(master)), collapse = "\n")
    fork_text <- paste(unlist(xlsx_raw_parts(fork)), collapse = "\n")
    expect_true(grepl(fx$note, master_text, fixed = TRUE), info = mode)
    for (secret in c(fx$note, so, "rev_b", "Word Author One", "Word Author Two")) {
      expect_false(grepl(secret, fork_text, fixed = TRUE), info = sprintf("%s mode: '%s' in the fork", mode, secret))
    }

    # One plain Documents sheet, no Documents_<id> sheets, in the fork only
    expect_false(any(grepl("^Documents_", openxlsx::getSheetNames(fork))), info = mode)
    expect_equal(
      names(readxl::read_excel(fork, sheet = "Documents", col_types = "text")),
      c(
        "file", "pipeline_stage", "comment_count", "resolved_comment_count",
        "reply_count", "tracked_change_count", "rev_a", "resolved"
      ),
      info = mode
    )
    if (mode == "per_reviewer") {
      expect_true(all(paste0("Documents_", c(fr, so)) %in% openxlsx::getSheetNames(master)))
    }
  }
})

test_that("a fork carries no sign-off columns and no Word author column on any sheet (A6-24)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  fx <- fork_fixture()
  res <- cbe_docx_review_extract(fx$input, fx$out, fork_reviewers = fx$fork, signoff_reviewers = fx$signoff)

  for (sheet in c("Comments", "SuggestedChanges", "TrackedChanges")) {
    master_cols <- names(openxlsx::read.xlsx(res$paths$master, sheet = sheet))
    fork_cols <- names(openxlsx::read.xlsx(res$paths$forks[["rev_a"]], sheet = sheet))
    expect_true("author" %in% master_cols, info = paste("master", sheet))
    expect_false("author" %in% fork_cols, info = paste("fork", sheet))
    expect_false(any(c("chair_x", "chair_x_comment", "rev_b", "rev_b_comment") %in% fork_cols), info = paste("fork", sheet))
  }
  expect_true(all(c("rev_a", "rev_a_comment") %in% names(openxlsx::read.xlsx(res$paths$forks[["rev_a"]], sheet = "Comments"))))
  # The master is unchanged: every reviewer's columns stay there
  expect_true(all(c("chair_x", "chair_x_comment", "rev_a", "rev_b") %in% names(openxlsx::read.xlsx(res$paths$master, sheet = "Comments"))))

  # reviewer_fork_drop_columns() is what decides it
  expect_true("author" %in% reviewer_fork_drop_columns("rev_a", fx$fork, fx$signoff))
})

test_that("the CSV exports leave out sign-off columns and authors unless review_config() asks for them (A6-24)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  read_csv_names <- function(path) names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  run <- function() {
    fx <- fork_fixture(parent.frame())
    res <- cbe_docx_review_extract(fx$input, fx$out, fork_reviewers = fx$fork, signoff_reviewers = fx$signoff)
    lapply(res$paths[c("comments_csv", "tracked_changes_csv", "suggested_changes_csv")], read_csv_names)
  }

  by_default <- run()
  for (csv in names(by_default)) {
    expect_false(any(c("author", "chair_x", "chair_x_comment") %in% by_default[[csv]]), info = csv)
  }
  expect_true(all(c("rev_a", "rev_a_comment", "rev_b", "resolved") %in% by_default$comments_csv))
  expect_true(all(c("rev_a", "rev_b", "resolved") %in% by_default$suggested_changes_csv))
  # Metadata the CSVs always carried is still there
  expect_true(all(c("file", "comment_id", "comment_text") %in% by_default$comments_csv))
  expect_true(all(c("file", "changed_text") %in% by_default$tracked_changes_csv))

  expect_equal(review_config(csv_reviewer_columns = "all")$csv_reviewer_columns, "all")
  everything <- run()
  expect_true(all(c("author", "chair_x", "chair_x_comment", "rev_a") %in% everything$comments_csv))
  expect_true("author" %in% everything$tracked_changes_csv)
  expect_true(all(c("author", "chair_x") %in% everything$suggested_changes_csv))

  review_config(csv_reviewer_columns = "none")
  nothing <- run()
  expect_false(any(c("author", "chair_x", "rev_a", "rev_a_comment", "rev_b") %in% nothing$comments_csv))
  expect_false(any(c("author", "chair_x", "rev_a", "rev_b") %in% nothing$suggested_changes_csv))
  expect_true("resolved" %in% nothing$comments_csv)

  expect_error(review_config(csv_reviewer_columns = "some"))
})

# =============================================================================
# A6-12: CSV CELLS THAT EXCEL WOULD RUN AS A FORMULA ARE NEUTRALISED
# =============================================================================

test_that("neutralise_csv_formulas() prefixes exactly the cells a spreadsheet would evaluate (A6-12)", {
  x <- c(
    "=1+1", "+1+1", "-2+3", "@SUM(1+1)", "  =padded", "\t=tab", "\r=cr", "\n=newline", "\t  +x",
    "plain", " plain", "a=b", "text-minus", "", NA, "'=already text", "1-2", "50%"
  )
  expect_equal(
    neutralise_csv_formulas(x),
    c(
      "'=1+1", "'+1+1", "'-2+3", "'@SUM(1+1)", "'  =padded", "'\t=tab", "'\r=cr", "'\n=newline", "'\t  +x",
      "plain", " plain", "a=b", "text-minus", "", NA, "'=already text", "1-2", "50%"
    )
  )
  expect_equal(neutralise_csv_formulas(c(1, 2)), c(1, 2))
  expect_equal(neutralise_csv_formulas(character(0)), character(0))
})

test_that("no CSV export holds a live formula, whether Word text or a reviewer's own note (A6-12)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  input <- withr::local_tempdir(pattern = "a6_csv_")
  build_review_docx(
    file.path(input, "doc_one.docx"),
    paragraphs = c("=HYPERLINK(\"http://x.example/\",\"sel\")", "plain two", "plain three", "plain four", "plain five"),
    comments = list(
      list(id = 0, author = "Word Author One", text = "=HYPERLINK(\"http://x.example/\",\"click\")", para = 1),
      list(id = 1, author = "Word Author One", text = "+1+1", para = 2),
      list(id = 2, author = "Word Author One", text = "-2+3", para = 3),
      list(id = 3, author = "Word Author One", text = "@SUM(1+1)", para = 4),
      list(id = 4, author = "Word Author One", text = "an ordinary note", para = 5)
    ),
    insertions = list(list(author = "Word Author Two", text = "=1+1 inserted", para = 5))
  )
  out <- file.path(input, "out")
  run <- function() cbe_docx_review_extract(input, out, fork_reviewers = "rev_a")
  res <- run()

  # A reviewer types a formula-looking note into their own comment column of the fork
  fork <- res$paths$forks[["rev_a"]]
  wb <- openxlsx::loadWorkbook(fork)
  at <- which(names(openxlsx::readWorkbook(wb, "Comments")) == "rev_a_comment")
  openxlsx::writeData(wb, "Comments", "=SUM(1,1) typed by the reviewer", startCol = at, startRow = 2)
  openxlsx::saveWorkbook(wb, fork, overwrite = TRUE)
  res <- run()

  read_cells <- function(path) {
    df <- utils::read.csv(path, colClasses = "character", na.strings = "NA", check.names = FALSE)
    unlist(df, use.names = FALSE)
  }
  for (csv in c("comments_csv", "tracked_changes_csv", "suggested_changes_csv")) {
    cells <- read_cells(res$paths[[csv]])
    cells <- cells[!is.na(cells)]
    expect_false(any(grepl("^[=+@-]", trimws(cells)) | grepl("^[\t\r]", cells)), info = csv)
  }

  comments <- utils::read.csv(res$paths$comments_csv, colClasses = "character", na.strings = "NA")
  expect_true("'=HYPERLINK(\"http://x.example/\",\"click\")" %in% comments$comment_text)
  expect_true(all(c("'+1+1", "'-2+3", "'@SUM(1+1)", "an ordinary note") %in% comments$comment_text))
  expect_true("'=SUM(1,1) typed by the reviewer" %in% comments$rev_a_comment)
  expect_true("'=1+1 inserted" %in% utils::read.csv(res$paths$tracked_changes_csv, colClasses = "character")$changed_text)

  # The workbooks keep the text as it is (a string cell is never a formula in xlsx)
  stored <- openxlsx::read.xlsx(res$paths$master, sheet = "Comments")$comment_text
  expect_true("=HYPERLINK(\"http://x.example/\",\"click\")" %in% stored)
  expect_true("+1+1" %in% stored)
})

# =============================================================================
# A6-13: STORED ERROR TEXT CARRIES NO LOCAL PATHS OR ACCOUNT NAMES
# =============================================================================

# Spellings of a folder as it can appear in an error message
path_spellings <- function(dir) {
  fwd <- sub("/+$", "", gsub("\\", "/", dir, fixed = TRUE))
  unique(c(fwd, gsub("/", "\\", fwd, fixed = TRUE)))
}
is_filesystem_root <- function(dir) grepl("^([A-Za-z]:)?[/\\\\]*$", dir)
contains_ci <- function(text, pattern) grepl(tolower(pattern), tolower(text), fixed = TRUE)

test_that("scrub_local_paths() replaces the temp, input and home folders in either slash style (A6-13)", {
  tmp <- tempdir()
  input <- file.path(withr::local_tempdir(pattern = "a6_in_"), "reviewed docs")
  home <- path.expand("~")

  msg <- sprintf(
    "cannot open file '%s/docx_1a2b/C:/x.txt': Invalid argument; also %s and %s; and the input %s/doc.docx",
    tmp, gsub("/", "\\", tmp, fixed = TRUE), gsub("/", "\\", home, fixed = TRUE), input
  )
  out <- scrub_local_paths(msg, input_dir = input)
  expect_match(out, "<tempdir>/docx_1a2b/C:/x.txt", fixed = TRUE)
  expect_match(out, "<input_dir>/doc.docx", fixed = TRUE)
  expect_match(out, "cannot open file", fixed = TRUE)
  expect_match(out, "Invalid argument", fixed = TRUE)
  for (secret in c(path_spellings(tmp), path_spellings(input))) {
    expect_false(contains_ci(out, secret), info = secret)
  }
  if (!is_filesystem_root(home)) {
    expect_match(out, "<home>", fixed = TRUE)
    for (secret in path_spellings(home)) {
      expect_false(contains_ci(out, secret), info = secret)
    }
  }

  # Text without a path, and a missing message, pass through
  expect_equal(scrub_local_paths("Couldn't find end of Start Tag oops [76]"), "Couldn't find end of Start Tag oops [76]")
  expect_equal(scrub_local_paths(character(0)), character(0))
  expect_equal(scrub_local_paths(NA_character_), NA_character_)
})

test_that("a failed document's stored error text names no local path, but verbose output stays complete (A6-13)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  input <- withr::local_tempdir(pattern = "a6_err_")
  build_review_docx(file.path(input, "good.docx"))
  build_review_docx(file.path(input, "broken.docx"))
  home <- path.expand("~")
  tmp <- tempdir()
  raw_msg <- sprintf(
    "cannot open file '%s/docx_9f8e/C:/Users/someone/x.txt': Invalid argument (profile %s, input %s)",
    tmp, home, input
  )

  real_extract <- extract_from_docx
  local_mocked_bindings(
    extract_from_docx = function(docx_path, ...) {
      if (basename(docx_path) == "broken.docx") stop(raw_msg, call. = FALSE)
      real_extract(docx_path, ...)
    },
    .package = "TempleCBE"
  )
  out <- file.path(input, "out")
  # a document that fails to extract is reported in one warning (A6-10), which names the file only
  expect_warning(
    res <- cbe_docx_review_extract(input, out, fork_reviewers = "rev_a"),
    "1 Word file\\(s\\) could not be read.*broken\\.docx"
  )

  expect_equal(res$errors$file, "broken.docx")
  secrets <- c(path_spellings(tmp), path_spellings(input), if (!is_filesystem_root(home)) path_spellings(home))
  for (secret in secrets) {
    expect_false(contains_ci(res$errors$error, secret), info = secret)
  }
  expect_match(res$errors$error, "cannot open file '<tempdir>/docx_9f8e/", fixed = TRUE)

  # Master and fork: the Errors sheet and the whole workbook are free of the paths
  for (workbook in c(res$paths$master, res$paths$forks[["rev_a"]])) {
    errors_sheet <- openxlsx::read.xlsx(workbook, sheet = "Errors")
    expect_equal(errors_sheet$file, "broken.docx")
    text <- paste(unlist(xlsx_raw_parts(workbook)), collapse = "\n")
    for (secret in secrets) {
      expect_false(contains_ci(text, secret), info = paste(basename(workbook), secret))
    }
  }

  # The console keeps the full message when asked to be verbose
  console <- character(0)
  withCallingHandlers(
    utils::capture.output(cbe_docx_review_extract(input, out, fork_reviewers = "rev_a", verbose = TRUE)),
    message = function(m) {
      console <<- c(console, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")  # the A6-10 warning, checked above
  )
  expect_true(any(grepl(raw_msg, console, fixed = TRUE)))
})

test_that("workbooks do not carry the operating-system account name as author (A6-13)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  fx <- fork_fixture()
  res <- cbe_docx_review_extract(fx$input, fx$out, fork_reviewers = fx$fork)
  account <- c(Sys.getenv("USERNAME"), Sys.getenv("USER"), Sys.info()[["user"]])
  account <- unique(account[nzchar(account)])

  for (workbook in c(res$paths$master, res$paths$forks[["rev_a"]])) {
    core <- xlsx_raw_parts(workbook)[["docProps/core.xml"]]
    expect_match(core, "<dc:creator>TempleCBE</dc:creator>", fixed = TRUE)
    expect_match(core, "<cp:lastModifiedBy>TempleCBE</cp:lastModifiedBy>", fixed = TRUE)
    for (name in account) {
      expect_false(grepl(sprintf(">%s<", name), core, fixed = TRUE), info = basename(workbook))
    }
  }
})

# =============================================================================
# A6-14: THE docXwalk SHEET SHOWS REAL, NON-ABSOLUTE PATHS
# =============================================================================

test_that("format_repo_path() is relative to the root on whole path components and never absolute (A6-14)", {
  base <- normalizePath(withr::local_tempdir(pattern = "a6_walk_"), winslash = "/")
  root <- file.path(base, "proj")
  for (d in c(file.path(root, "reviews", "round_1"), file.path(base, "proj_old"), file.path(base, "elsewhere"))) {
    dir.create(d, recursive = TRUE)
  }

  inside <- file.path(root, "reviews", "round_1", "a.docx")
  expect_equal(format_repo_path(inside, root = root), "proj\\reviews\\round_1\\a.docx")
  expect_equal(format_repo_path(inside, root = paste0(root, "/")), "proj\\reviews\\round_1\\a.docx")

  # A sibling that merely starts with the root's name is not inside it (and its
  # path must not be cut in the middle of a folder name)
  expect_equal(format_repo_path(file.path(base, "proj_old", "b.docx"), root = root), "b.docx")
  # Outside the root: the file name, not the absolute path with the user profile
  outside <- format_repo_path(file.path(base, "elsewhere", "c.docx"), root = root)
  expect_equal(outside, "c.docx")
  expect_false(grepl(basename(base), outside, fixed = TRUE))

  expect_true(is.na(format_repo_path(NA_character_, root = root)))
  expect_true(is.na(format_repo_path("", root = root)))
})

test_that("build_docxwalk_df() reports the folder a document was read from, and file names only for forks (A6-14)", {
  base <- normalizePath(withr::local_tempdir(pattern = "a6_walk_"), winslash = "/")
  root <- file.path(base, "proj")
  dir.create(file.path(root, "reviews"), recursive = TRUE)
  dir.create(file.path(base, "elsewhere"))
  in_root <- file.path(root, "reviews", "a.docx")
  outside <- file.path(base, "elsewhere", "b.docx")

  walk <- function(...) {
    build_docxwalk_df(
      c("a.docx", "b.docx", "gone.docx"), catalog = list(), qmd_index = character(0),
      doc_paths = c(in_root, outside), repo_root = root, ...
    )
  }
  master <- walk()
  # The real folder, not an invented fixed folder; outside the root and for a
  # prior-round document no longer in the folder: just the name
  expect_equal(master$file, c("proj\\reviews\\a.docx", "b.docx", "gone.docx"))
  expect_false(any(grepl("tasks", master$file, fixed = TRUE)))

  fork <- walk(basename_only = TRUE)
  expect_equal(fork$file, c("a.docx", "b.docx", "gone.docx"))
  expect_false(any(grepl("[/\\\\]", unlist(fork))))

  # QMD source and rendered outputs follow the same rule
  qmd_dir <- file.path(root, "src")
  dir.create(qmd_dir)
  file.create(file.path(qmd_dir, c("a.qmd", "a.pdf", "a.docx")))
  index <- c(a = file.path(qmd_dir, "a.qmd"))
  with_qmd <- function(...) {
    build_docxwalk_df("a.docx", catalog = list(), qmd_index = index, doc_paths = in_root, repo_root = root, ...)
  }
  expect_equal(with_qmd()$source_qmd, "proj\\src\\a.qmd")
  expect_equal(with_qmd()$rendered_pdf, "proj\\src\\a.pdf")
  expect_equal(with_qmd(basename_only = TRUE)$source_qmd, "a.qmd")
  expect_equal(with_qmd(basename_only = TRUE)$rendered_docx, "a.docx")
})

test_that("the docXwalk sheets of the master and of a fork never hold an absolute path (A6-14)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  # Documents far from the working directory and outside any project folder
  fx <- fork_fixture()
  elsewhere <- withr::local_tempdir(pattern = "a6_cwd_")
  withr::local_dir(elsewhere)
  res <- cbe_docx_review_extract(fx$input, fx$out, fork_reviewers = fx$fork)

  for (workbook in c(res$paths$master, res$paths$forks[["rev_a"]])) {
    walk <- openxlsx::read.xlsx(workbook, sheet = "docXwalk")
    expect_equal(walk$file, "doc_one.docx", info = basename(workbook))
    expect_false(any(grepl("tasks", unlist(walk), fixed = TRUE)), info = basename(workbook))
  }
  expect_equal(res$docxwalk$file, "doc_one.docx")
})

# =============================================================================
# A6-23: NO STUDY STRUCTURE IS BUILT INTO THE PACKAGE; IT IS CONFIGURATION
# =============================================================================

test_that("by default the extractor knows no pipeline stage, alias or folder (A6-23)", {
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  cfg <- review_config()
  expect_equal(cfg$pipeline_catalog, list())
  expect_equal(cfg$stem_aliases, character(0))
  expect_null(cfg$analysis_dir)
  expect_equal(cfg$input_dirs, character(0))

  # Nothing to look up: no stage, so nothing for the fuzzy matcher to mis-assign
  catalog <- load_pipeline_catalog(NULL)
  expect_length(catalog, 0)
  for (f in c("intro.docx", "eda_ABC_11_21_25.docx", "alpha_step_typo.docx", "survival.docx", "anything_else.docx")) {
    m <- match_docx_to_pipeline(f, catalog)
    expect_equal(m$rank, 999L, info = f)
    expect_equal(m$stage, "Unknown / Extra", info = f)
  }

  # No manifest or source index is searched for, even where one exists
  base <- withr::local_tempdir(pattern = "a6_cfg_")
  dir.create(file.path(base, "analysis", "run1"), recursive = TRUE)
  file.create(file.path(base, "analysis", "run1", c("reports_to_render.xlsx", "alpha_step.qmd")))
  withr::local_dir(base)
  withr::local_envvar(c(TEMPLECBE_ANALYSIS_ROOT = NA))
  expect_null(find_pipeline_manifest(base))
  expect_length(build_qmd_index(), 0)

  # The default input folder is the working directory, whatever folders exist
  dir.create(file.path(base, "reviews", "incoming"), recursive = TRUE)
  build_review_docx(file.path(base, "reviews", "incoming", "doc_one.docx"))
  expect_equal(normalizePath(find_default_review_input_dir()), normalizePath(base))
})

test_that("review_config() validates the pipeline, alias and folder settings and rejects a bad call whole (A6-23)", {
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  # A catalog is a list of stages or a data frame; stems are stored in lower case
  from_list <- review_config(pipeline_catalog = list(
    list(stem = "Alpha_Step", heading = 1, name = "Alpha"),
    list(stem = "beta_step", stage = "Second stage")
  ))$pipeline_catalog
  expect_equal(vapply(from_list, function(e) e$stem, ""), c("alpha_step", "beta_step"))
  expect_equal(from_list[[2]]$stage, "Second stage")

  from_df <- review_config(pipeline_catalog = data.frame(
    stem = c("one_step", "two_step"), heading = c(1, NA), name = c("One", "Two")
  ))$pipeline_catalog
  expect_equal(vapply(from_df, function(e) e$stem, ""), c("one_step", "two_step"))
  expect_null(from_df[[2]]$heading)

  expect_equal(review_config(stem_aliases = c(Typo_Step = "Alpha_Step"))$stem_aliases, c(typo_step = "alpha_step"))
  expect_equal(review_config(analysis_dir = "analysis/run1")$analysis_dir, "analysis/run1")
  expect_equal(review_config(input_dirs = c("a/b", "c"))$input_dirs, c("a/b", "c"))

  expect_error(review_config(pipeline_catalog = "alpha_step"), "list of stages")
  expect_error(review_config(pipeline_catalog = list(list(name = "no stem"))), "needs a `stem`")
  expect_error(review_config(pipeline_catalog = list(list(stem = "a_step"), list(stem = "A_STEP"))), "more than once")
  expect_error(review_config(stem_aliases = c("alpha_step")), "named character vector")
  expect_error(review_config(analysis_dir = c("a", "b")), "single folder name")
  expect_error(review_config(input_dirs = c("a", NA)), "folder names")

  # A call with one bad argument changes nothing, not even its valid arguments
  expect_error(review_config(analysis_dir = "elsewhere", pipeline_catalog = "bad"))
  expect_equal(review_config()$analysis_dir, "analysis/run1")

  # Each can be cleared again
  review_config(pipeline_catalog = list(), stem_aliases = character(0), analysis_dir = character(0), input_dirs = character(0))
  cleared <- review_config()
  expect_equal(cleared$pipeline_catalog, list())
  expect_equal(cleared$stem_aliases, character(0))
  expect_null(cleared$analysis_dir)
  expect_equal(cleared$input_dirs, character(0))
})

test_that("the configured analysis folder and input folders are the only places looked in (A6-23)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  base <- normalizePath(withr::local_tempdir(pattern = "a6_cfg_"), winslash = "/")
  withr::local_dir(base)
  withr::local_envvar(c(TEMPLECBE_ANALYSIS_ROOT = NA))
  src <- file.path(base, "analysis", "run1")
  dir.create(file.path(src, "archive"), recursive = TRUE)
  openxlsx::write.xlsx(
    data.frame(file = "src/gamma_step.qmd", Heading = 3, name = "Gamma Step"),
    file.path(src, "reports_to_render.xlsx")
  )
  file.create(file.path(src, "alpha_step.qmd"), file.path(src, "archive", "old_step.qmd"))

  review_config(analysis_dir = "analysis/run1")
  manifest <- normalizePath(find_pipeline_manifest(base), winslash = "/")
  expect_equal(manifest, file.path(src, "reports_to_render.xlsx"))
  # Also found from a folder below the project (its parents are tried)
  dir.create(file.path(base, "work", "deep"), recursive = TRUE)
  expect_equal(normalizePath(find_pipeline_manifest(file.path(base, "work", "deep")), winslash = "/"), manifest)
  # The manifest's stages come first, then the configured ones
  review_config(pipeline_catalog = list(list(stem = "alpha_step", name = "Alpha")))
  catalog <- load_pipeline_catalog(manifest)
  expect_equal(names(catalog), c("gamma_step", "alpha_step"))
  expect_equal(catalog$gamma_step$stage, "Heading 3: Gamma Step")
  # Sources are indexed from the same folder (archives excluded)
  expect_equal(names(build_qmd_index()), "alpha_step")
  # An absolute folder works too
  review_config(analysis_dir = src)
  expect_equal(normalizePath(find_pipeline_manifest(NULL), winslash = "/"), manifest)

  # Default input folder: the first configured folder that holds documents
  dir.create(file.path(base, "reviews", "incoming"), recursive = TRUE)
  dir.create(file.path(base, "reviews", "empty"), recursive = TRUE)
  build_review_docx(file.path(base, "reviews", "incoming", "doc_one.docx"))
  review_config(input_dirs = c("reviews/missing", "reviews/empty", "reviews/incoming"))
  expect_equal(
    normalizePath(find_default_review_input_dir(), winslash = "/"),
    file.path(base, "reviews", "incoming")
  )
  expect_equal(
    normalizePath(find_default_review_input_dir(input_dirs = file.path(base, "reviews", "incoming")), winslash = "/"),
    file.path(base, "reviews", "incoming")
  )
})

test_that("documents are labelled and ordered by the configured stages, and are unknown without them (A6-23)", {
  skip_if_not_installed("openxlsx")
  old <- review_options_reset()
  on.exit(options(old), add = TRUE)

  input <- withr::local_tempdir(pattern = "a6_stages_")
  build_review_docx(
    file.path(input, "delta_step.docx"),
    comments = list(list(id = 0, author = "Word Author One", text = "note on delta", para = 1))
  )
  build_review_docx(
    file.path(input, "omega_step.docx"),
    comments = list(list(id = 0, author = "Word Author One", text = "note on omega", para = 1))
  )
  run <- function(out) cbe_docx_review_extract(input, file.path(input, out), fork_reviewers = "rev_a", verbose = TRUE)
  quietly <- function(code) {
    console <- character(0)
    res <- withCallingHandlers(
      utils::capture.output(value <- code),
      message = function(m) {
        console <<- c(console, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    list(value = value, console = console)
  }

  # Nothing configured: every document is "Unknown / Extra", in name order
  plain <- quietly(run("out_plain"))
  expect_true(any(grepl("No pipeline catalog is configured", plain$console, fixed = TRUE)))
  expect_equal(plain$value$documents$file, c("delta_step.docx", "omega_step.docx"))
  expect_equal(plain$value$documents$pipeline_stage, rep("Unknown / Extra", 2))
  expect_equal(unname(plain$value$comments$pipeline_stage), rep("Unknown / Extra", 2))

  # Configured: the catalog's order and labels win over the alphabet
  review_config(pipeline_catalog = list(
    list(stem = "omega_step", heading = 1, name = "Omega Stage"),
    list(stem = "delta_step", heading = 2, name = "Delta Stage")
  ))
  staged <- quietly(run("out_staged"))
  expect_true(any(grepl("pipeline catalog from review_config() (2 stages)", staged$console, fixed = TRUE)))
  expect_equal(staged$value$documents$file, c("omega_step.docx", "delta_step.docx"))
  expect_equal(staged$value$documents$pipeline_stage, c("Heading 1: Omega Stage", "Heading 2: Delta Stage"))
  expect_equal(unname(staged$value$comments$pipeline_stage), c("Heading 1: Omega Stage", "Heading 2: Delta Stage"))
  expect_equal(
    openxlsx::read.xlsx(staged$value$paths$master, sheet = "Documents")$pipeline_stage,
    c("Heading 1: Omega Stage", "Heading 2: Delta Stage")
  )
})
