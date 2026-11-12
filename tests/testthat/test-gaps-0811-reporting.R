# Tests for the fresh-review reporting/rendering/publishing fixes (items 85-132
# of TempleCBE-gaps-and-features-2026-11-11.md, Part 1 section C). Each test
# names the item it covers. Tests that need a Suggests package or an external
# tool skip when it is missing, as in the other test files; tests that only
# exercise validation run everywhere.
#
# Tests run sequentially (see AGENTS.md); nothing here starts a parallel
# worker or touches the random stream.

gaps_t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")

# Write `text` to `name` in `dir` with a modification time `hours` after gaps_t0
gaps_make_file <- function(dir, name, text = name, hours = 0) {
  p <- file.path(dir, name)
  writeLines(text, p)
  Sys.setFileTime(p, gaps_t0 + hours * 3600)
  p
}

# A temp directory with forward slashes on every platform
gaps_tmp_dir <- function(env = parent.frame()) {
  normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
}

# A minimal .qmd template with the given YAML lines, in `dir`
gaps_qmd <- function(dir, name = "report.qmd", yaml = c("title: Test", "format: html"), body = "Hello.") {
  p <- file.path(dir, name)
  writeLines(c("---", yaml, "---", "", body), p)
  p
}

# A minimal .docx whose word/document.xml holds `body_xml` (paragraph XML).
# Needs the zip package, as in test-docx_qa.R.
gaps_docx <- function(body_xml, env = parent.frame()) {
  testthat::skip_if_not_installed("zip")
  td <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(td, "word"))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="xml" ContentType="application/xml"/></Types>'
  ), file.path(td, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>',
    body_xml, "</w:body></w:document>"
  ), file.path(td, "word", "document.xml"), useBytes = TRUE)
  target <- file.path(withr::local_tempdir(.local_envir = env), "doc.docx")
  zip::zip(target, files = list.files(td, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = td)
  target
}

gaps_para <- function(text) paste0("<w:p><w:r><w:t>", text, "</w:t></w:r></w:p>")

# ---- compute_graph: staleness (85) -------------------------------------------------

test_that("is_stale() treats a deleted source as stale, and the render plan lists it (85)", {
  d <- gaps_tmp_dir()
  gaps_make_file(d, "make.R", hours = 5)
  gaps_make_file(d, "fresh.csv", hours = 10)
  prod <- FileOutputs("make", file.path(d, "make.R"), renders = FALSE)
  out <- FilePath("fresh", file.path(d, "fresh.csv"))
  expect_false(is_stale(out, prod))
  unlink(file.path(d, "make.R"))
  expect_true(is_stale(out, prod))

  # the same, for a renderable report: the missing source is out of date, not unchanged
  qmd <- gaps_qmd(d, "report.qmd", yaml = c("title: R", "format: html"))
  csv <- gaps_make_file(d, "raw.csv", hours = 1)
  obj <- create_qmd_renderer(name = "report", path = qmd, deps = list(FilePath("raw", csv)), output_format = "html")
  unlink(csv)
  expect_warning(
    plan <- get_render_plan(list(obj)),
    "were not found on disk"
  )
  expect_true("report" %in% names(plan))
})

test_that("is_stale() treats a renderer with no outputs as stale, but not a bare script (88)", {
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "make.R", hours = 5)
  # a renderer that declares nothing has never produced anything: out of date
  expect_true(is_stale(FileOutputs("r", p, renders = TRUE)))
  # a non-rendering script with no declared outputs has nothing to rebuild
  expect_false(is_stale(FileOutputs("s", p, renders = FALSE)))
})

test_that("refresh_pipeline() records a deleted file as a missing timestamp (85)", {
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "make.R", hours = 5)
  prod <- FileOutputs("make", p, renders = FALSE)
  expect_false(is.na(prod@mtime))
  unlink(p)
  refreshed <- refresh_pipeline(prod)
  expect_true(is.na(refreshed@mtime))
})

test_that("hash staleness treats a deleted recorded source as stale (85)", {
  d <- gaps_tmp_dir()
  gaps_make_file(d, "raw.csv", "a,b\n1,2", hours = 1)
  gaps_make_file(d, "make.R", "write.csv(1)", hours = 2)
  gaps_make_file(d, "out.csv", "x\n1", hours = 3)
  raw <- FilePath("raw", file.path(d, "raw.csv"))
  out <- FilePath("out", file.path(d, "out.csv"))
  prod <- FileOutputs("make", file.path(d, "make.R"), dependencies = list(raw),
                      output = list(out), renders = FALSE)
  hf <- file.path(d, "hashes.json")
  record_pipeline_hashes(list(raw, prod), hash_file = hf)
  expect_false(is_stale(out, prod, stale_by = "hash", hash_file = hf))

  unlink(file.path(d, "raw.csv"))
  expect_true(is_stale(out, prod, stale_by = "hash", hash_file = hf))
})

# ---- compute_graph: pipeline_config (98, 99) ---------------------------------------

test_that("pipeline_config() restore puts back the configuration a value represents (98, 99)", {
  before <- pipeline_config()
  on.exit(pipeline_config(restore = before), add = TRUE)

  cfg_a <- pipeline_config(study_name = "Study A", default_formats = "html")
  expect_equal(pipeline_config()$study_name, "Study A")
  expect_equal(attr(cfg_a, "previous")$pipeline.study_name, before$study_name)

  cfg_b <- pipeline_config(study_name = "Study B")
  expect_equal(pipeline_config()$study_name, "Study B")

  pipeline_config(restore = cfg_a)
  expect_equal(pipeline_config()$study_name, "Study A")
  expect_equal(pipeline_config()$default_formats, "html")

  # restore of a restore works: the value carries the state it represents
  pipeline_config(restore = cfg_b)
  expect_equal(pipeline_config()$study_name, "Study B")

  expect_error(pipeline_config(restore = list(study_name = "X")), "must be a value returned by")
  expect_error(pipeline_config(restore = cfg_a, study_name = "Y"), "cannot be combined")
})

test_that("pipeline_config() validates the values it is given (99)", {
  before <- pipeline_config()
  on.exit(pipeline_config(restore = before), add = TRUE)

  expect_error(pipeline_config(study_name = c("a", "b")), "single non-empty character string")
  expect_error(pipeline_config(study_name = ""), "single non-empty character string")
  expect_error(pipeline_config(default_formats = "bogus"), "Unknown output format")
  expect_error(pipeline_config(default_formats = character(0)), "non-empty")
  expect_error(pipeline_config(stage_labels = c("a", "b")), "non-empty name for every entry")
  expect_error(pipeline_config(stage_labels = c(x = "")), "single non-empty character string")
  expect_error(pipeline_config(reset = "yes"), "must be `TRUE` or `FALSE`")

  # a single NA clears one setting and leaves the others alone
  pipeline_config(study_name = "Keep Me", default_formats = "html")
  pipeline_config(study_name = NA)
  expect_equal(pipeline_config()$study_name, before$study_name)
  expect_equal(pipeline_config()$default_formats, "html")
})

# ---- compute_graph: config defaults (101), escaping (118, 119), interactive (120, 121) ----

test_that("create_qmd_renderer() takes its default format from pipeline_config() (101)", {
  before <- pipeline_config()
  on.exit(pipeline_config(restore = before), add = TRUE)
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = "title: No format declared")

  pipeline_config(default_formats = "html")
  r <- create_qmd_renderer(name = "r", path = qmd)
  expect_equal(vapply(r@output, function(o) o@path, character(1)), sub("\\.qmd$", ".html", qmd))

  # an explicit NULL still means "extract the formats from the YAML"
  pipeline_config(default_formats = "docx")
  r2 <- create_qmd_renderer(name = "r2", path = qmd, output_format = NULL)
  expect_equal(vapply(r2@output, function(o) o@path, character(1)), sub("\\.qmd$", ".pdf", qmd))
})

test_that("visualize_pipeline() escapes quotes and backslashes in DOT output (118)", {
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "a.csv")
  obj <- FilePath('we"ird\\name', p)
  code <- visualize_pipeline(list(obj), extract_graph_code = TRUE)
  expect_match(code, 'we\\"ird\\\\name', fixed = TRUE)
  expect_no_match(code, '"we"ird', fixed = TRUE)
})

test_that("as_igraph() escapes HTML in tooltips (119)", {
  skip_if_not_installed("igraph")
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "a.csv")
  obj <- FilePath("a<b>&\"c\"", p, description = "5 < 6 & 7 > 2")
  g <- as_igraph(list(obj))
  titles <- igraph::vertex_attr(g, "title")
  # the values are escaped (the tooltip itself uses <b> markup for its labels)
  expect_match(titles[1], "a&lt;b&gt;&amp;&quot;c&quot;", fixed = TRUE)
  expect_match(titles[1], "5 &lt; 6 &amp; 7 &gt; 2", fixed = TRUE)
})

test_that("visualize_pipeline_interactive() validates direction (120)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("visNetwork")
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "a.csv")
  objs <- list(FilePath("a", p))

  expect_error(visualize_pipeline_interactive(objs, direction = "sideways"), "direction")
  expect_warning(visualize_pipeline_interactive(objs, hierarchical = FALSE, direction = "UD"), "ignored")
})

test_that("collapse_by_stage() guards its input (120)", {
  skip_if_not_installed("igraph")
  expect_error(collapse_by_stage("not a graph"), "igraph")
  empty <- igraph::make_empty_graph(0)
  expect_identical(collapse_by_stage(empty), empty)
  plain <- igraph::make_ring(3)
  expect_error(collapse_by_stage(plain), "stage")
  with_stage <- plain
  igraph::V(with_stage)$stage <- c("A", NA, "B")
  collapsed <- collapse_by_stage(with_stage)
  expect_true("Other" %in% igraph::V(collapsed)$stage)
})

test_that("export_subgraph() warns when stage is ignored and stops without one (120)", {
  skip_if_not_installed("igraph")
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "a.csv")
  g <- as_igraph(list(FilePath("a", p)))
  plain <- igraph::make_ring(3)

  expect_error(export_subgraph(plain, stage = "A"), "no vertex attribute `stage`")

  # the warning fires before the export itself, which needs visNetwork: the
  # error after it (or the written file, where visNetwork is installed) is
  # not what this checks
  warned <- NULL
  tryCatch(
    withCallingHandlers(
      export_subgraph(g, focal_node = "a", stage = "A", file = file.path(d, "sub.html")),
      warning = function(w) { warned <<- conditionMessage(w); invokeRestart("muffleWarning") }
    ),
    error = function(e) NULL
  )
  expect_match(warned, "ignored")
})

test_that("shared outputs are recognised by canonical path, not by spelling (121)", {
  d <- gaps_tmp_dir()
  p <- gaps_make_file(d, "shared.csv")
  spelled <- file.path(d, "sub", "..", "shared.csv")
  a <- FileOutputs("a", file.path(d, "a.R"), output = list(FilePath("shared", p)), renders = FALSE)
  b <- FileOutputs("b", file.path(d, "b.R"), output = list(FilePath("shared", spelled)), renders = FALSE)
  # the same file under two spellings is one shared output, so one warning
  warns <- character(0)
  withCallingHandlers(
    .warn_shared_outputs(list(a, b)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_length(warns, 1L)
  expect_match(warns[1], "shared", fixed = TRUE)
})

# ---- render (86, 87, 89, 90) ------------------------------------------------------------

test_that("a single fresh FileOutputs is planned by render_pipeline(only_stale = TRUE) (86)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = c("title: Fresh", "format: html"))
  obj <- create_qmd_renderer(name = "fresh", path = qmd, output_format = "html")
  res <- render_pipeline(obj, only_stale = TRUE, dry_run = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$file[1], "report.qmd")
  expect_equal(res$status[1], "dry_run")
})

test_that("render_pipeline() does not crash on an NA artifact_role (87)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d)
  # an output whose role was never set: artifact_role is NA, not "deliverable_report"
  obj <- FileOutputs("report", qmd,
                     output = list(FilePath("report HTML", sub("\\.qmd$", ".html", qmd))),
                     renders = TRUE)
  res <- render_pipeline(obj, dry_run = TRUE)
  expect_s3_class(res, "data.frame")
})

test_that("render_pipeline(formats = 'yaml') falls back to pdf/docx when the YAML declares none (87)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = "title: No format declared")
  res <- render_pipeline(qmd, formats = "yaml", dry_run = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$output[1], "pdf, docx")
  expect_equal(res$status[1], "dry_run")
})

test_that("render_pipeline() stops when no formats resolve (89)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d)
  expect_error(render_pipeline(qmd, formats = character(0), dry_run = TRUE), "No output formats resolved")
})

test_that("render_pipeline() validates workers up front (90)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d)
  expect_error(render_pipeline(qmd, workers = 1.5, dry_run = TRUE), "workers")
  expect_error(render_pipeline(qmd, workers = 0, dry_run = TRUE), "workers")
})

# ---- review extract (100, 113-117) -------------------------------------------------------

test_that("review_config() validates reset (100)", {
  old_fork <- getOption("review.fork_reviewers")
  on.exit(options(review.fork_reviewers = old_fork), add = TRUE)

  expect_error(review_config(reset = "yes"), "must be `TRUE` or `FALSE`")
  expect_error(review_config(reset = NA), "must be `TRUE` or `FALSE`")
  # reset clears what was set, and a rejected call changes nothing
  review_config(fork_reviewers = "AB")
  expect_error(review_config(reset = "yes"), "must be `TRUE` or `FALSE`")
  expect_equal(review_config()$fork_reviewers, "AB")
  review_config(reset = TRUE)
  expect_equal(review_config()$fork_reviewers, c("reviewer_1", "reviewer_2", "reviewer_3"))
})

test_that("normalize_flag_cell() accepts the truthy/falsy sets and warns on the rest (114)", {
  expect_equal(
    normalize_flag_cell(c("TRUE", "yes", "Y", "1", "done", "FALSE", "no", "0", "", NA)),
    c("TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "", "")
  )
  expect_warning(
    out <- normalize_flag_cell(c("ok", "maybe")),
    "OK, MAYBE"
  )
  expect_equal(out, c("FALSE", "FALSE"))
})

test_that("load_existing_tracker() warns instead of silently treating an unreadable tracker as empty (113)", {
  skip_if_not_installed("readxl")
  d <- gaps_tmp_dir()
  bad <- file.path(d, "tracker.xlsx")
  writeLines("this is not a workbook", bad)
  expect_warning(
    res <- load_existing_tracker(bad),
    "Could not list the sheets"
  )
  expect_equal(nrow(res$comments), 0)
})

test_that("backup_review_file() warns and returns NULL when the backup cannot be made (113)", {
  d <- gaps_tmp_dir()
  f <- file.path(d, "tracker.xlsx")
  writeLines("x", f)
  # "backups" exists as a file, so the backup directory cannot be created
  writeLines("blocker", file.path(d, "backups"))
  expect_warning(
    res <- backup_review_file(f),
    "Could not create the backup directory"
  )
  expect_null(res)
})

test_that("validate_review_tracker() rejects a non-xlsx/csv path and validates sheet (115)", {
  d <- gaps_tmp_dir()
  txt <- file.path(d, "tracker.txt")
  writeLines("x", txt)
  expect_error(validate_review_tracker(txt), "must be a path to a `.xlsx` or `.csv`")
  csv <- file.path(d, "tracker.csv")
  writeLines(c("status,location,rationale", "done,,ok"), csv)
  expect_error(validate_review_tracker(csv, sheet = c("a", "b")), "single non-empty sheet name")
})

test_that("validate_review_tracker() accepts an unknown status with a rationale (115)", {
  d <- gaps_tmp_dir()
  csv <- file.path(d, "tracker.csv")
  writeLines(c(
    "status,location,rationale",
    "needs-work,,waiting on the author",
    "adopted,page 2,",
    "mystery,,has a rationale",
    "mystery2,,"
  ), csv)
  # the unknown statuses with a location or a rationale are fine; only the
  # one with neither is an issue (a location alone would once have passed it)
  res <- validate_review_tracker(csv, error_on_invalid = FALSE)
  expect_false(res$valid)
  expect_equal(res$n_invalid, 1L)
  expect_equal(res$issues$row, 4L)
  expect_match(res$issues$reason[1], "missing both a location and a rationale")

  csv2 <- file.path(d, "tracker2.csv")
  writeLines(c(
    "status,location,rationale",
    "needs-work,,waiting on the author",
    "adopted,page 2,",
    "mystery,,has a rationale"
  ), csv2)
  expect_true(validate_review_tracker(csv2)$valid)
})

test_that("validate_review_tracker() prefers the Review Tracker sheet (115)", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  d <- gaps_tmp_dir()
  xlsx <- file.path(d, "tracker.xlsx")
  good <- data.frame(status = "adopted", location = "p1", rationale = "", stringsAsFactors = FALSE)
  other <- data.frame(status = "bogus", location = "", rationale = "", stringsAsFactors = FALSE)
  writexl::write_xlsx(list("Other" = other, "Review Tracker" = good), path = xlsx)
  res <- validate_review_tracker(xlsx)
  expect_true(res$valid)
  expect_error(validate_review_tracker(xlsx, sheet = "Missing"), "not found in")
  expect_false(validate_review_tracker(xlsx, sheet = "Other", error_on_invalid = FALSE)$valid)
})

test_that("write_review_csv() writes a UTF-8 BOM (116)", {
  d <- gaps_tmp_dir()
  p <- file.path(d, "review.csv")
  write_review_csv(data.frame(a = "caf\u00e9", stringsAsFactors = FALSE), p)
  bytes <- readBin(p, what = "raw", n = 3)
  expect_equal(bytes, as.raw(c(0xef, 0xbb, 0xbf)))
  expect_true(grepl("caf", readLines(p, encoding = "UTF-8", warn = FALSE)[2], fixed = TRUE))
})

test_that("write_tracker_docx() breaks lines with <w:br/> instead of a literal newline (116)", {
  skip_if_not_installed("zip")
  d <- gaps_tmp_dir()
  p <- file.path(d, "tracker.docx")
  write_tracker_docx(
    data.frame(Item = c("first\nsecond", "plain"), Status = c("open", "done"), stringsAsFactors = FALSE),
    p, "Title\nWith Break"
  )
  expect_true(file.exists(p))
  td <- gaps_tmp_dir()
  zip::unzip(p, files = "word/document.xml", exdir = td)
  xml <- paste(readLines(file.path(td, "word", "document.xml"), warn = FALSE), collapse = "\n")
  expect_match(xml, "<w:br/>", fixed = TRUE)
  expect_no_match(xml, "<w:t[^>]*>[^<]*\n", perl = TRUE)
})

# ---- zip_render / zip_reports (102-106) --------------------------------------------------

test_that(".resolve_all_formats() resolves 'all' to the YAML-declared formats (104)", {
  d <- gaps_tmp_dir()
  docx_qmd <- gaps_qmd(d, "a.qmd", yaml = c("title: A", "format: docx"))
  plain_qmd <- gaps_qmd(d, "b.qmd", yaml = "title: B")
  expect_equal(.resolve_all_formats("all", docx_qmd), "docx")
  expect_equal(.resolve_all_formats("all", plain_qmd), "html")
  expect_equal(.resolve_all_formats(c("all", "pdf"), docx_qmd), c("pdf", "docx"))
  expect_equal(.resolve_all_formats(c("html", "pdf"), docx_qmd), c("html", "pdf"))
})

test_that("zip_render() refuses a zip_name with a path separator (106)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d)
  expect_error(zip_render(qmd, zip_name = "sub/out.zip"), "must be a file name")
})

test_that("zip_reports() swaps .Rmd and uppercase .QMD extensions for outputs (103)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("zip")
  d <- gaps_tmp_dir()
  writeLines("x", file.path(d, "report.Rmd"))
  writeLines("pdf", file.path(d, "report.pdf"))
  writeLines("x", file.path(d, "UPPER.QMD"))
  writeLines("pdf", file.path(d, "UPPER.pdf"))
  reports <- data.frame(
    name = c("Rmd report", "Upper report"),
    path = c(file.path(d, "report.Rmd"), file.path(d, "UPPER.QMD")),
    stage = c("01", "01"), description = c("", ""),
    stringsAsFactors = FALSE
  )
  z <- zip_reports(reports, output_formats = "pdf", output_dir = file.path(d, "out"), zip_name = "reports.zip")
  expect_true(file.exists(z))
  contents <- zip::zip_list(z)$filename
  expect_true(any(grepl("report\\.pdf$", contents)))
  expect_true(any(grepl("UPPER\\.pdf$", contents)))
})

test_that("zip_reports() validates its reports argument (102)", {
  skip_if_not_installed("openxlsx")
  expect_error(zip_reports(data.frame(name = character(), path = character())), "has no rows")
  expect_error(zip_reports(data.frame(x = 1)), "columns `name` and `path`")
})

# ---- bundle_deliverables (107-109) --------------------------------------------------------

test_that("diff_deliverables() refuses a zip with a symbolic-link entry (109)", {
  skip_if_not_installed("zip")
  skip_if(Sys.which("python3") == "", "python3 is needed to build a zip with a symlink entry")
  d <- gaps_tmp_dir()
  good <- gaps_docx(gaps_para("Hello."))
  evil <- file.path(d, "evil.docx")
  outside <- file.path(d, "outside")
  dir.create(outside)
  py <- file.path(d, "make_evil.py")
  writeLines(c(
    "import zipfile, sys",
    "out, target = sys.argv[1], sys.argv[2]",
    "z = zipfile.ZipFile(out, 'w')",
    "info = zipfile.ZipInfo('escape')",
    "info.create_system = 3",
    "info.external_attr = 0o120777 << 16",
    "z.writestr(info, target)",
    "z.writestr('escape/pwned.txt', 'pwned')",
    "z.writestr('word/document.xml', '<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body><w:p><w:r><w:t>Hi.</w:t></w:r></w:p></w:body></w:document>')",
    "z.writestr('[Content_Types].xml', '<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"xml\" ContentType=\"application/xml\"/></Types>')",
    "z.close()"
  ), py)
  code <- suppressWarnings(system2("python3", c(py, evil, outside), stdout = FALSE, stderr = FALSE))
  expect_equal(code, 0)
  expect_true("symlink" %in% zip::zip_list(evil)$type)
  expect_error(diff_deliverables(evil, good), "symbolic link")
  expect_false(file.exists(file.path(outside, "pwned.txt")))
})

test_that("diff_deliverables() returns a uniform schema with no NA counts (108)", {
  skip_if_not_installed("zip")
  old <- gaps_docx(paste0(gaps_para("One."), gaps_para("Two.")))
  new <- gaps_docx(paste0(gaps_para("One."), gaps_para("Two changed."), gaps_para("Three.")))
  res <- diff_deliverables(old, new)
  expect_true(all(c("file", "status", "hash_changed", "text_changed", "n_added", "n_removed",
                    "n_edited", "n_unpaired", "budget_exceeded", "diff_summary", "diff_lines",
                    "section_summary", "largest_changes") %in% names(res)))
  # the zip also holds [Content_Types].xml, which is unchanged: the document row is the one to check
  expect_equal(nrow(res), 2)
  doc <- res[res$file == "word/document.xml", ]
  expect_equal(nrow(doc), 1)
  expect_equal(doc$status[1], "modified")
  expect_false(anyNA(doc$n_added) || anyNA(doc$n_removed) || anyNA(doc$n_edited) || anyNA(doc$n_unpaired))
  expect_false(doc$budget_exceeded[1])
  expect_true(all(c("section", "edited", "added", "removed", "unpaired") %in% names(doc$section_summary[[1]])))
})

test_that("pair_paragraphs_by_similarity() reports the unpaired counts when the budget is exceeded (108)", {
  old <- paste0("old paragraph ", seq_len(60))
  new <- paste0("new paragraph ", seq_len(60))
  res <- pair_paragraphs_by_similarity(old, new, time_budget_sec = 0)
  expect_true(res$budget_exceeded)
  expect_equal(res$n_added, 60L)
  expect_equal(res$n_removed, 60L)
  expect_equal(res$n_unpaired, 120L)
})

test_that("package_deliverables() validates output_formats (107)", {
  expect_error(package_deliverables(output_formats = "bogus"), "Unknown output format")
  expect_error(package_deliverables(output_formats = character(0)), "non-empty character vector")
})

# ---- pdf_to_docx (110-112) ---------------------------------------------------------------

# The toolchain as check_docx_toolchain() reports it when python + pdf2docx is
# usable; the paths are fake, so a test using this also mocks the converters.
gaps_python_toolchain <- function() {
  list(
    python_with_pdf2docx = "fake_python",
    python_any = "fake_python",
    soffice = NULL,
    word_com = FALSE,
    backend = "python"
  )
}

# Mock the converter toolchain for the rest of the test (mirrors
# local_toolchain() in test-pdf_to_docx.R): check_docx_toolchain() reports
# python + pdf2docx as usable, and .convert_python() behaves as given.
gaps_mock_converters <- function(convert_python, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_docx_toolchain = function(...) gaps_python_toolchain(),
    .convert_python = convert_python,
    .env = .env
  )
}

gaps_capture_warnings <- function(expr) {
  warns <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  list(value = value, warnings = warns)
}

test_that("convert_pdfs_to_docx() counts only attempted rows in its final warning (110)", {
  gaps_mock_converters(function(py, pdf, docx, timeout = 600) FALSE)
  d <- gaps_tmp_dir()
  good <- file.path(d, "good.pdf")
  writeLines("%PDF-1.4", good)
  conv <- data.frame(
    src = c(good, file.path(d, "missing.pdf")),
    dest = c(file.path(d, "good.docx"), file.path(d, "missing.docx")),
    stringsAsFactors = FALSE
  )
  # the missing row is invalid and never attempted: the summary counts the one row tried
  out <- gaps_capture_warnings(convert_pdfs_to_docx(conv, backend = "python", strict = FALSE))
  expect_equal(out$value$converted, c(FALSE, FALSE))
  expect_true(any(grepl("1 of 1 PDF -> DOCX conversion(s) failed", out$warnings, fixed = TRUE)))
  expect_false(any(grepl("2 of 2|1 of 2", out$warnings)))

  # nothing attempted: no summary warning (the skipped row still warns by itself)
  out2 <- gaps_capture_warnings(convert_pdfs_to_docx(conv[2, , drop = FALSE], backend = "python", strict = FALSE))
  expect_equal(out2$value$converted, FALSE)
  expect_false(any(grepl("conversion(s) failed", out2$warnings, fixed = TRUE)))
})

test_that("convert_pdfs_to_docx() reports a failed temp_dest copy as not converted (111)", {
  gaps_mock_converters(function(py, pdf, docx, timeout = 600) { file.create(docx); TRUE })
  d <- gaps_tmp_dir()
  pdf <- file.path(d, "a.pdf")
  writeLines("%PDF-1.4", pdf)
  blocker <- file.path(d, "blocker")
  writeLines("x", blocker)
  conv <- data.frame(
    src = pdf, dest = file.path(d, "a.docx"),
    temp_dest = file.path(blocker, "a.docx"),
    stringsAsFactors = FALSE
  )
  out <- gaps_capture_warnings(convert_pdfs_to_docx(conv, backend = "python", strict = FALSE))
  expect_false(out$value$converted[1])
  expect_true(any(grepl("could not copy the DOCX to temp_dest", out$warnings, fixed = TRUE)))
})

test_that("find_soffice() resolves a bare name from the option via PATH (112)", {
  skip_on_os("windows") # a bare name needs an .exe on Windows; a shell script is not found
  d <- gaps_tmp_dir()
  fake <- file.path(d, "soffice")
  writeLines("#!/bin/sh\nexit 0", fake)
  Sys.chmod(fake, "0755")
  withr::local_envvar(c(PATH = paste(d, Sys.getenv("PATH"), sep = .Platform$path.sep)))
  withr::local_options(templecbe.soffice = "soffice")
  expect_equal(find_soffice(), fake)
})

# ---- clean_publish (122-124) -------------------------------------------------------------

test_that("guard_public_remote() stops when the hook refuses an unfenced push too (122)", {
  skip_if_no_git()
  fx <- cp_fixture()
  # a second, unfenced remote
  bare2 <- file.path(fx$base, "other.git")
  dir.create(bare2)
  cp_git(bare2, "init", "--bare", "--quiet")
  cp_git(fx$repo, "remote", "add", "upstream", bare2)
  # a hook that refuses every push, fenced or not
  testthat::local_mocked_bindings(
    .cp_write_hook = function(plan) {
      writeLines(c("#!/bin/sh", "echo 'clean_publish guard: push refused: is guarded.' >&2", "exit 1"), plan$path)
      Sys.chmod(plan$path, "0755")
    }
  )
  expect_error(cp_quiet(guard_public_remote(fx$remote, repo_root = fx$repo)), "blocking pushes it should allow")
})

test_that("parse_clean_publish_args() takes one --allow-ignored path per flag (123)", {
  args <- parse_clean_publish_args(c("--allow-ignored", "a,b.csv", "--allow-ignored", "c.csv"))
  expect_equal(args$allow_ignored, c("a,b.csv", "c.csv"))
  expect_false(any(grepl("comma-separated", clean_publish_usage(), fixed = TRUE)))
})

test_that(".cp_unquote_c() undoes git's C-quoting (123)", {
  expect_equal(.cp_unquote_c('"a\\tb"'), "a\tb")
  expect_equal(.cp_unquote_c('"a\\"b"'), 'a"b')
  expect_equal(.cp_unquote_c('"a\\nb"'), "a\nb")
  expect_equal(.cp_unquote_c('"a\\\\b"'), "a\\b")
  expect_equal(charToRaw(.cp_unquote_c('"caf\\303\\251"')), charToRaw("caf\u00e9"))
  expect_equal(.cp_unquote_c("plain.csv"), "plain.csv")
})

test_that(".cp_git() runs git and reports its status (124)", {
  skip_if_no_git()
  fx <- cp_fixture()
  res <- .cp_git(fx$repo, c("rev-parse", "HEAD"))
  expect_equal(res$status, 0L)
  expect_true(nzchar(res$output[1]))
  bad <- .cp_git(fx$repo, c("rev-parse", "no-such-ref"), error_ok = TRUE)
  expect_false(bad$status == 0L)
  expect_error(.cp_git(fx$repo, c("rev-parse", "no-such-ref")), "failed")
})

test_that("clean_publish() records whether the push happened (124)", {
  skip_if_no_git()
  fx <- cp_fixture()
  cp_quiet(clean_publish(repo_root = fx$repo, push = FALSE))
  expect_false(.cp_state$pushed)
  cp_quiet(clean_publish(repo_root = fx$repo, remote = "origin", push = TRUE))
  expect_true(.cp_state$pushed)
})

# ---- docx_qa (125) -------------------------------------------------------------------------

test_that(".docx_body_paragraphs() reports an unreadable file plainly (125)", {
  d <- gaps_tmp_dir()
  bad <- file.path(d, "not.docx")
  writeLines("not a zip", bad)
  expect_error(extract_docx_qa(bad), "could not be read as a Word document")
})

test_that("extract_docx_qa() measures the whole HYPERLINK formula against 255 (125)", {
  skip_if_not_installed("zip")
  d <- gaps_tmp_dir()
  body <- paste0(gaps_para("What is the answer?"), gaps_para("See model_report.pdf."))
  path <- gaps_docx(body)
  # a 240-character path fits the old path-only check, but the formula
  # =HYPERLINK("path","model_report.pdf") is 273 characters, so it must not link
  long_path <- paste0(strrep("d/", 112), "model_report.pdf")
  expect_equal(nchar(long_path), 240)
  index <- data.frame(file = "model_report.pdf", path = long_path, stringsAsFactors = FALSE)
  expect_warning(
    out <- extract_docx_qa(path, index_data = index),
    "more than 255 characters"
  )
  expect_equal(length(out$links[[1]]), 0)
  expect_equal(out$unresolved_citations[1], "model_report.pdf")

  # a short path does link
  short <- data.frame(file = "model_report.pdf", path = "reports/model_report.pdf", stringsAsFactors = FALSE)
  out2 <- extract_docx_qa(path, index_data = short)
  expect_equal(length(out2$links[[1]]), 1)
  expect_match(out2$links[[1]][1], "HYPERLINK", fixed = TRUE)
})

# ---- write_workbook (126) ------------------------------------------------------------------

test_that("write_database_to_excel() validates path, sheets and elements (126)", {
  expect_error(write_database_to_excel(list(a = data.frame(x = 1)), path = "a.csv"), "must end in `.xlsx`")
  expect_error(write_database_to_excel(list(a = data.frame(x = 1), b = 1:3), path = "a.xlsx"), "must be a data frame")
  expect_error(write_database_to_excel(list(a = data.frame(x = 1)), path = "a.xlsx", metadata_sheet = NA), "single non-empty string")
  expect_error(write_database_to_excel(stats::setNames(list(data.frame(x = 1)), NA), path = "a.xlsx"), "must not be NA")
})

test_that("write_database_to_excel() creates the parent directory (126)", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  d <- gaps_tmp_dir()
  p <- file.path(d, "new", "sub", "db.xlsx")
  write_database_to_excel(list(labs = data.frame(id = 1:2)), path = p, include_metadata = FALSE)
  expect_true(file.exists(p))
  expect_equal(readxl::excel_sheets(p), "labs")
})

test_that("write_database_metadata() accepts only csv/xlsx/xls (126)", {
  expect_error(write_database_metadata(data.frame(a = 1), "a.txt"), "must end in `.csv`, `.xlsx` or `.xls`")
})

test_that("apply_database_metadata() validates metadata and keeps the data-frame name (126)", {
  df <- data.frame(id = 1:2, age = c(20, 30))
  meta <- tibble::tibble(dataset_name = "df", columns = c("id", "age"), labels = c("ID", "Age"))
  out <- apply_database_metadata(df, meta)
  expect_equal(as.character(labelled::var_label(out$age)), "Age")
  expect_error(apply_database_metadata(df, "not a df"), "must be a data frame")
  expect_identical(apply_database_metadata(df, NULL), df)
})

# ---- run_sas_script (127) ------------------------------------------------------------------

test_that("run_sas_script() requires a .sas extension (127)", {
  d <- gaps_tmp_dir()
  txt <- file.path(d, "prog.txt")
  writeLines("data _null_; run;", txt)
  expect_error(run_sas_script(txt), "must end in `.sas`")
})

# ---- theme_cbe / temple_brand (128) ---------------------------------------------------------

test_that("the formatters validate digits and p-values (128)", {
  expect_error(fmt_pct(0.5, digits = 1.5), "single non-negative whole number")
  expect_error(fmt_pct(1.5), "between 0 and 1")
  expect_equal(fmt_pct(NA), "NA%")
  expect_equal(fmt_pct(0.5, digits = 0), "50%")
  expect_error(fmt_num(1, digits = -1), "single non-negative whole number")
  expect_error(fmt_sig(1, digits = "3"), "single non-negative whole number")
  expect_error(format_p_value(1.2), "between 0 and 1")
  expect_equal(format_p_value(c(0.04, NA)), c("0.040", NA_character_))
  expect_error(fmt_hr(1.25, 1.05, 1.48, digits = 2.5), "single non-negative whole number")
  expect_equal(fmt_hr(1.25, 1.05, 1.48), "HR 1.25 (95% CI 1.05\u20131.48)")
})

test_that("temple_pal() validates n (128)", {
  expect_error(temple_pal()(1.5), "single whole number")
  expect_error(temple_pal()(NA), "single whole number")
  expect_error(temple_pal()(-1), "single whole number")
  expect_equal(length(temple_pal()(0)), 0)
  expect_equal(length(temple_pal("diverging")(5)), 5)
  expect_error(temple_pal("diverging")(2.5), "single whole number")
})

test_that(".check_extension_source() takes a local path before a GitHub slug (128)", {
  expect_equal(.check_extension_source("jkylearmstrong-temple/quarto_temple_brand"), "jkylearmstrong-temple/quarto_temple_brand")
  d <- gaps_tmp_dir()
  expect_equal(.check_extension_source(d), d)
  slug <- file.path(d, "owner", "repo")
  dir.create(slug, recursive = TRUE)
  expect_equal(withr::with_dir(d, .check_extension_source("owner/repo")), "owner/repo")
  expect_warning(.check_extension_source("https://example.com/ext.zip"), "downloaded and installed as it is")
  expect_error(.check_extension_source("http://example.com/ext.zip"), "not a GitHub source")
  expect_error(.check_extension_source("not a valid source!!"), "not a GitHub source")
})

test_that("use_temple_brand() refuses a _quarto.yml that is a directory (128)", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("withr")
  d <- gaps_tmp_dir()
  dir.create(file.path(d, "_quarto.yml"))
  expect_error(use_temple_brand(d, check_root = FALSE), "is a directory, not a file")
})

# ---- render_variants / parameterized pair / study (129) -------------------------------------

test_that(".rv_declared_params() reads past a byte-order mark (129)", {
  d <- gaps_tmp_dir()
  f <- file.path(d, "t.qmd")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("---\ntitle: T\nparams:\n  a: 1\n  b: 2\n---\n\ntext\n")), f)
  expect_equal(.rv_declared_params(f), c("a", "b"))
})

test_that("render_variants() allows case-differing variant names on a case-sensitive file system (129)", {
  skip_if(.case_insensitive_fs(), "case-differing names collide on this file system")
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = c("title: T", "params:", "  a: 1"))
  res <- render_variants(qmd, variants = list(A = list(a = 1), a = list(a = 2)), formats = "html", dry_run = TRUE)
  expect_equal(nrow(res), 2)
  expect_error(
    render_variants(qmd, variants = list(A = list(a = 1), A = list(a = 2)), formats = "html", dry_run = TRUE),
    "must be unique"
  )
})

test_that("render_parameterized_pair() allows case-differing output names on a case-sensitive file system (129)", {
  skip_if(.case_insensitive_fs(), "case-differing names collide on this file system")
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = c("title: T", "params:", "  a: 1"))
  res <- render_parameterized_pair(qmd, params_original = list(a = 1), params_imputed = list(a = 2),
                                   output_names = c("Original", "original"), dry_run = TRUE)
  expect_equal(nrow(res), 2)
})

test_that("render_parameterized_study() returns an empty plan for an empty folder with dry_run (129)", {
  d <- gaps_tmp_dir()
  empty <- file.path(d, "empty")
  dir.create(empty)
  res <- render_parameterized_study(empty, target_dirs = file.path(d, "out"), param_list = list(list(a = 1)), dry_run = TRUE)
  expect_equal(nrow(res), 0)
  expect_equal(names(res), c("document", "target_dir", "variant", "format", "params", "output", "status", "error"))
  expect_error(
    render_parameterized_study(empty, target_dirs = file.path(d, "out"), param_list = list(list(a = 1))),
    "No document matching"
  )
})

test_that("render_parameterized_study() refuses a target folder inside the source folder (129)", {
  d <- gaps_tmp_dir()
  src <- file.path(d, "src")
  dir.create(src)
  gaps_qmd(src, "doc.qmd", yaml = c("title: T", "params:", "  a: 1"))
  expect_error(
    render_parameterized_study(src, target_dirs = file.path(src, "out"), param_list = list(list(a = 1))),
    "inside it"
  )
  expect_error(
    render_parameterized_study(src, target_dirs = src, param_list = list(list(a = 1))),
    "inside it"
  )
  # a dry run plans without writing, so the check does not apply; a sibling is fine
  res <- render_parameterized_study(src, target_dirs = file.path(src, "out"), param_list = list(list(a = 1)), dry_run = TRUE)
  expect_equal(nrow(res), 1)
  res2 <- render_parameterized_study(src, target_dirs = file.path(d, "out"), param_list = list(list(a = 1)), dry_run = TRUE)
  expect_equal(nrow(res2), 1)
})

test_that("render_variants() gives each call a distinct private run id (129)", {
  d <- gaps_tmp_dir()
  qmd <- gaps_qmd(d, yaml = c("title: T", "params:", "  a: 1"))
  seen <- character(0)
  stub <- function(input, output_format, output_file, execute_params, quiet, metadata = NULL) {
    seen <<- c(seen, output_file)
    file.create(file.path(dirname(input), output_file))
  }
  render_variants(qmd, variants = list(v1 = list(a = 1)), formats = "html", renderer = stub)
  render_variants(qmd, variants = list(v1 = list(a = 1)), formats = "html", renderer = stub)
  expect_length(seen, 2)
  expect_false(identical(seen[1], seen[2]))
})

# ---- atomic_io (131) -----------------------------------------------------------------------

test_that("stale-lock cleanup does not consume the random stream (131)", {
  d <- gaps_tmp_dir()
  lock_dir <- file.path(d, "test.lock")
  dir.create(lock_dir)
  # a lock whose owner is long dead, aged past stale_age
  writeLines(c("pid: 999999", "time: 2026-01-01 00:00:00"), file.path(lock_dir, "lock_owner"))
  Sys.setFileTime(lock_dir, Sys.time() - 60)
  set.seed(42)
  before <- .Random.seed
  res <- NULL
  expect_warning(
    res <- with_file_lock(lock_dir, { 42 }, timeout = 5, stale_age = 1),
    "Removing stale advisory lock"
  )
  expect_equal(res, 42)
  expect_identical(.Random.seed, before)
  expect_false(dir.exists(lock_dir))
})

test_that("validate_secrets_dir() creates the directory with mode 0700 (131)", {
  skip_if(.Platform$OS.type == "windows", "POSIX permissions do not apply on Windows")
  d <- gaps_tmp_dir()
  out <- validate_secrets_dir(file.path(d, "secrets"))
  expect_true(dir.exists(out))
  expect_equal(bitwAnd(as.integer(file.info(out)$mode), 511L), 448L) # 0700
})

# ---- remove_dir_tree (132) -------------------------------------------------------------------

test_that("remove_dir_tree() leaves the tree and warns when the link scan fails (132)", {
  d <- gaps_tmp_dir()
  tree <- file.path(d, "tree")
  dir.create(tree)
  writeLines("x", file.path(tree, "f.txt"))
  testthat::local_mocked_bindings(dir_ls = function(...) stop("cannot list"), .package = "fs")
  expect_warning(
    gone <- remove_dir_tree(tree),
    "leaving the directory in place"
  )
  expect_false(gone)
  expect_true(dir.exists(tree))
  expect_true(file.exists(file.path(tree, "f.txt")))
})

test_that(".is_pid_alive() sees live processes psnice() cannot query on Windows (131)", {
  skip_if_not(.Platform$OS.type == "windows", "Windows-only process table")
  expect_true(.is_pid_alive(Sys.getpid()))
  # pid 4 is the Windows "System" process: psnice() returns NA for it
  expect_true(.windows_pid_listed(4L))
  expect_true(.is_pid_alive(4L))
  expect_false(.is_pid_alive(999999L))
})
