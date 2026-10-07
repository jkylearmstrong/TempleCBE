test_that("zip_reports errors clearly without a 'name'/'path' column", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  expect_error(zip_reports(data.frame(name = "x")), "name.*path")
})

test_that("zip_reports bundles existing outputs, deliverables, and an index", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")

  tmp_dir <- file.path(tempdir(), "test_zip_reports")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Fake already-rendered outputs -- zip_reports never renders anything
  # itself, it just checks for files next to the .qmd source.
  writeLines("qmd source", file.path(tmp_dir, "intro.qmd"))
  writeLines("pdf bytes", file.path(tmp_dir, "intro.pdf"))
  writeLines("docx bytes", file.path(tmp_dir, "intro.docx"))

  writeLines("qmd source", file.path(tmp_dir, "results.qmd"))
  writeLines("pdf bytes", file.path(tmp_dir, "results.pdf"))
  # deliberately no results.docx -- exercises the "missing docx" path

  deliverable <- file.path(tmp_dir, "extra_data.csv")
  writeLines("a,b\n1,2", deliverable)

  reports <- data.frame(
    name = c("Introduction", "Results"),
    path = c(file.path(tmp_dir, "intro.qmd"), file.path(tmp_dir, "results.qmd")),
    stage = c("00_intro", "01_results"),
    description = c("overview", "findings"),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tmp_dir, "out")
  zip_path <- zip_reports(
    reports,
    output_formats = c("pdf", "docx"),
    data_deliverables = deliverable,
    zip_name = "bundle.zip",
    output_dir = out_dir
  )

  expect_true(file.exists(zip_path))
  expect_equal(basename(zip_path), "bundle.zip")

  zip_contents <- utils::unzip(zip_path, list = TRUE)$Name
  expect_true(any(grepl("pdf/00_intro/intro\\.pdf$", zip_contents)))
  expect_true(any(grepl("docx/00_intro/intro\\.docx$", zip_contents)))
  expect_true(any(grepl("pdf/01_results/results\\.pdf$", zip_contents)))
  expect_false(any(grepl("01_results/results\\.docx$", zip_contents)))
  expect_true(any(grepl("^data/extra_data\\.csv$", zip_contents)))
  expect_true(any(grepl("^report_order\\.xlsx$", zip_contents)))
})

test_that("zip_reports disambiguates same-stem reports from different folders", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")

  tmp_dir <- file.path(tempdir(), "test_zip_reports_same_stem")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  analysis_dirs <- file.path(tmp_dir, c("analysis1", "analysis2"))
  lapply(analysis_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  for (d in analysis_dirs) {
    writeLines("qmd source", file.path(d, "analysis.qmd"))
    writeLines("pdf bytes", file.path(d, "analysis.pdf"))
  }

  reports <- data.frame(
    name = c("Analysis 1", "Analysis 2"),
    path = file.path(analysis_dirs, "analysis.qmd"),
    stage = c("01_analysis", "01_analysis"),
    stringsAsFactors = FALSE
  )

  out_dir <- file.path(tmp_dir, "out")
  zip_path <- zip_reports(reports, output_formats = "pdf", output_dir = out_dir)
  zip_contents <- utils::unzip(zip_path, list = TRUE)$Name

  expect_true(any(grepl("pdf/01_analysis/analysis1__analysis\\.pdf$", zip_contents)))
  expect_true(any(grepl("pdf/01_analysis/analysis2__analysis\\.pdf$", zip_contents)))
  expect_equal(length(grep("^pdf/01_analysis/.*analysis\\.pdf$", zip_contents)), 2)
})

test_that("zip_reports calls docx_from_pdf to fill in a missing DOCX", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")

  tmp_dir <- file.path(tempdir(), "test_zip_reports_docx_gen")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("qmd source", file.path(tmp_dir, "report.qmd"))
  writeLines("pdf bytes", file.path(tmp_dir, "report.pdf"))

  calls <- list()
  fake_converter <- function(src_pdf, dest_docx) {
    calls[[length(calls) + 1]] <<- list(src = src_pdf, dest = dest_docx)
    writeLines("generated docx", dest_docx)
  }

  reports <- data.frame(name = "Report", path = file.path(tmp_dir, "report.qmd"), stringsAsFactors = FALSE)
  out_dir <- file.path(tmp_dir, "out")

  zip_path <- zip_reports(reports, output_formats = "docx", output_dir = out_dir, docx_from_pdf = fake_converter)

  expect_length(calls, 1)
  expect_true(file.exists(file.path(tmp_dir, "report.docx")))
  zip_contents <- utils::unzip(zip_path, list = TRUE)$Name
  expect_true(any(grepl("docx/99_Other/report\\.docx$", zip_contents)))
})

# A report with its source, PDF and (optionally) DOCX in a fresh folder. The
# DOCX is made a day older than the PDF when `stale = TRUE`.
local_report_folder <- function(docx = c("none", "fresh", "stale"), .env = parent.frame()) {
  docx <- match.arg(docx)
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines("qmd source", file.path(dir, "report.qmd"))
  writeLines("NEW PDF", file.path(dir, "report.pdf"))
  if (docx != "none") {
    writeLines("OLD DOCX", file.path(dir, "report.docx"))
    if (docx == "stale") Sys.setFileTime(file.path(dir, "report.docx"), Sys.time() - 86400)
  }
  list(
    dir = dir,
    reports = data.frame(name = "Report", path = file.path(dir, "report.qmd"), stringsAsFactors = FALSE),
    out = file.path(dir, "out")
  )
}

# The "Report Index" sheet of a zip made by zip_reports().
zip_index <- function(zip_path) {
  ex <- withr::local_tempdir(.local_envir = parent.frame())
  utils::unzip(zip_path, files = "report_order.xlsx", exdir = ex)
  openxlsx::read.xlsx(file.path(ex, "report_order.xlsx"), sheet = "Report Index", sep.names = " ")
}

zip_entries <- function(zip_path) utils::unzip(zip_path, list = TRUE)$Name

test_that("a DOCX older than its PDF is not shipped when there is no docx_from_pdf, and a warning says so", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("stale")

  expect_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out)),
    "DOCX for 'Report'.*older than its PDF"
  )
  entries <- zip_entries(zip_path)
  expect_true("pdf/99_Other/report.pdf" %in% entries)
  expect_false(any(grepl("^docx/", entries)))
  expect_equal(zip_index(zip_path)$`DOCX Link`, "not converted")
  # The stale file itself is left alone on disk.
  expect_equal(readLines(file.path(rep$dir, "report.docx")), "OLD DOCX")
})

test_that("a failed docx_from_pdf does not let the stale DOCX through", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")

  # It reports failure (as convert_pdf_to_docx() does) and the old file is still there.
  rep <- local_report_folder("stale")
  calls <- 0L
  expect_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                                            docx_from_pdf = function(src_pdf, dest_docx) { calls <<- calls + 1L; FALSE })),
    "did not produce a current DOCX for 'Report'"
  )
  expect_equal(calls, 1L)
  expect_false(any(grepl("^docx/", zip_entries(zip_path))))
  expect_equal(zip_index(zip_path)$`DOCX Link`, "not converted")

  # It returns nothing and writes nothing: the file is still older than the PDF.
  rep <- local_report_folder("stale")
  expect_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                                            docx_from_pdf = function(src_pdf, dest_docx) invisible(NULL))),
    "did not produce a current DOCX"
  )
  expect_false(any(grepl("^docx/", zip_entries(zip_path))))

  # It claims failure but leaves a fresh file: the return value is believed.
  rep <- local_report_folder("stale")
  expect_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                                            docx_from_pdf = function(src_pdf, dest_docx) { writeLines("NEW", dest_docx); FALSE })),
    "did not produce a current DOCX"
  )
  expect_false(any(grepl("^docx/", zip_entries(zip_path))))
})

test_that("a docx_from_pdf that succeeds replaces the stale DOCX, whatever it returns", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")

  for (returns in list(NULL, TRUE, "path/to/dest.docx")) {
    rep <- local_report_folder("stale")
    zip_path <- NULL
    expect_no_warning(
      zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                                              docx_from_pdf = function(src_pdf, dest_docx) { writeLines("NEW DOCX", dest_docx); returns }))
    )
    expect_true("docx/99_Other/report.docx" %in% zip_entries(zip_path))
    ex <- withr::local_tempdir()
    utils::unzip(zip_path, exdir = ex)
    expect_equal(readLines(file.path(ex, "docx", "99_Other", "report.docx")), "NEW DOCX")
    expect_false(identical(zip_index(zip_path)$`DOCX Link`, "not converted"))
  }
})

test_that("a current DOCX is shipped as it is and docx_from_pdf is not called", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("fresh")   # written after the PDF

  calls <- 0L
  expect_no_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                                            docx_from_pdf = function(src_pdf, dest_docx) { calls <<- calls + 1L; FALSE }))
  )
  expect_equal(calls, 0L)
  expect_true("docx/99_Other/report.docx" %in% zip_entries(zip_path))
})

test_that("a missing DOCX without docx_from_pdf is skipped silently and marked in the index", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  old_wd <- getwd()

  expect_no_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out))
  )
  expect_false(any(grepl("^docx/", zip_entries(zip_path))))
  expect_equal(zip_index(zip_path)$`DOCX Link`, "not converted")
  expect_equal(getwd(), old_wd)
})

test_that("a DOCX with no PDF to compare against is shipped", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("stale")
  file.remove(file.path(rep$dir, "report.pdf"))

  expect_no_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, "docx", output_dir = rep$out))
  )
  expect_true("docx/99_Other/report.docx" %in% zip_entries(zip_path))
})

test_that("data deliverables that share a file name are all shipped, the later ones renamed, with a warning", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  dir.create(file.path(rep$dir, "a"))
  dir.create(file.path(rep$dir, "b"))
  dir.create(file.path(rep$dir, "c"))
  writeLines("A-RESULTS", file.path(rep$dir, "a", "results.csv"))
  writeLines("B-RESULTS", file.path(rep$dir, "b", "results.csv"))
  writeLines("C-RESULTS", file.path(rep$dir, "c", "RESULTS.CSV"))   # the same name on Windows and macOS

  w <- testthat::capture_warnings(
    zip_path <- suppressMessages(zip_reports(
      rep$reports, "pdf", output_dir = rep$out,
      data_deliverables = file.path(rep$dir, c("a", "b", "c"), c("results.csv", "results.csv", "RESULTS.CSV"))
    ))
  )
  expect_length(w, 2L)
  expect_match(w[1], "share the file name 'results.csv'.*results_2.csv")
  expect_match(w[2], "share the file name 'RESULTS.CSV'")

  data_entries <- grep("^data/", zip_entries(zip_path), value = TRUE)
  expect_length(data_entries, 3L)
  expect_true(all(c("data/results.csv", "data/results_2.csv") %in% data_entries))
  ex <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = ex)
  payloads <- vapply(file.path(ex, data_entries), function(f) readLines(f, warn = FALSE)[1], character(1))
  expect_setequal(unname(payloads), c("A-RESULTS", "B-RESULTS", "C-RESULTS"))
  # The first one keeps its own name, as before.
  expect_equal(readLines(file.path(ex, "data", "results.csv")), "A-RESULTS")
})

test_that("a data deliverable that cannot be copied is reported, and a missing one is still skipped quietly", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  good <- file.path(rep$dir, "good.csv")
  writeLines("a,b", good)
  a_folder <- file.path(rep$dir, "not_a_file")
  dir.create(a_folder)

  expect_warning(
    zip_path <- suppressMessages(zip_reports(
      rep$reports, "pdf", output_dir = rep$out,
      data_deliverables = c(good, a_folder, file.path(rep$dir, "missing.csv"))
    )),
    "Could not copy the data deliverable '.*not_a_file'"
  )
  expect_setequal(grep("^data/", zip_entries(zip_path), value = TRUE), "data/good.csv")
})

test_that("a report file that cannot be copied is reported and left out of the index", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  # file.exists() is TRUE for a folder, and a folder cannot be copied as a file.
  unlink(file.path(rep$dir, "report.pdf"))
  dir.create(file.path(rep$dir, "report.pdf"))

  expect_warning(
    zip_path <- suppressMessages(zip_reports(rep$reports, "pdf", output_dir = rep$out)),
    "Could not copy '.*report.pdf' into the zip"
  )
  expect_false(any(grepl("^pdf/", zip_entries(zip_path))))
  expect_true(is.na(zip_index(zip_path)$`PDF Link`))
})

# Folders named report_build_* that are in tempdir() right now.
staging_dirs <- function() list.files(tempdir(), pattern = "^report_build_")

test_that("a stage that climbs out of the build folder stays inside it", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  # Three levels up from <build>/pdf/<stage> is the folder that holds tempdir().
  marker <- paste0("escape_marker_", as.integer(runif(1, 1e6, 1e7)))
  outside <- file.path(dirname(tempdir()), marker)
  withr::defer(unlink(outside, recursive = TRUE))
  rep$reports$stage <- paste0("../../../", marker)

  zip_path <- suppressMessages(zip_reports(rep$reports, "pdf", output_dir = rep$out))

  expect_false(dir.exists(outside))
  # The report is in the zip, under the cleaned stage, and the index says where it is.
  expect_true(paste0("pdf/", marker, "/report.pdf") %in% zip_entries(zip_path))
  expect_equal(zip_index(zip_path)$Stage, rep$reports$stage)
})

test_that("safe_stage_dir keeps ordinary stage names and cleans only what could escape or not be a folder name", {
  expect_identical(safe_stage_dir("01_intro", "99_Other"), "01_intro")
  expect_identical(safe_stage_dir("01 Primary results", "99_Other"), "01 Primary results")
  accented <- paste0(intToUtf8(0xC9), "tape 2") # E with an acute accent, built so that this file stays ASCII
  expect_identical(safe_stage_dir(accented, "99_Other"), accented)
  expect_identical(safe_stage_dir("a/b\\c", "99_Other"), "a-b-c")
  expect_identical(safe_stage_dir("../../x", "99_Other"), "x")
  expect_identical(safe_stage_dir("C:/data", "99_Other"), "C-data")
  expect_identical(safe_stage_dir("what?*", "99_Other"), "what")
  expect_identical(safe_stage_dir(".hidden", "99_Other"), "hidden")
  expect_identical(safe_stage_dir("..", "99_Other"), "99_Other")
  expect_identical(safe_stage_dir(".", "99_Other"), "99_Other")
  expect_identical(safe_stage_dir("", "99_Other"), "99_Other")
  expect_identical(safe_stage_dir(NA_character_, "Reports"), "Reports")
  expect_identical(safe_stage_dir(character(0), "Reports"), "Reports")
})

test_that("zip_reports removes its staging folder, also when it fails", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  old_wd <- getwd()
  # The staging folder is named after the second it was made in, so a pause
  # keeps this call's folder from sharing a name with (and so replacing) one
  # an earlier test left behind: it is only what the call itself adds that counts.
  new_staging <- function(before) setdiff(staging_dirs(), before)

  rep <- local_report_folder("none")
  before <- staging_dirs()
  Sys.sleep(1.1)
  suppressMessages(zip_reports(rep$reports, "pdf", output_dir = rep$out))
  expect_equal(new_staging(before), character(0))
  expect_equal(getwd(), old_wd)

  # A failure half way (here: the DOCX callback stops) leaves nothing behind either.
  rep <- local_report_folder("stale")
  before <- staging_dirs()
  Sys.sleep(1.1)
  expect_error(
    zip_reports(rep$reports, c("pdf", "docx"), output_dir = rep$out,
                docx_from_pdf = function(src_pdf, dest_docx) stop("no converter")),
    "no converter"
  )
  expect_equal(new_staging(before), character(0))
  expect_equal(getwd(), old_wd)
})

test_that("a relative output_dir puts the zip in the caller's folder", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx package not available")
  rep <- local_report_folder("none")
  withr::local_dir(rep$dir)

  zip_path <- suppressMessages(zip_reports(rep$reports, "pdf", output_dir = "deliveries"))
  expect_true(file.exists(file.path(rep$dir, "deliveries", basename(zip_path))))
  expect_equal(normalizePath(dirname(zip_path)), normalizePath(file.path(rep$dir, "deliveries")))
})
