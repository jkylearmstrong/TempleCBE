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
