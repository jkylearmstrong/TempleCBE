test_that("audit_report_deliverables scans files and identifies tokens", {
  tmp <- withr::local_tempdir()

  script_content <- c(
    "df <- readRDS('data/sample_input.rds')",
    "writexl::write_xlsx(summary_tab, 'deliverables/summary.xlsx')",
    "x <- 10"
  )
  writeLines(script_content, file.path(tmp, "analysis_script.R"))

  res <- audit_report_deliverables(tmp)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("data/sample_input.rds" %in% res$token)
  expect_true("deliverables/summary.xlsx" %in% res$token)
  expect_false(all(res$exists_on_disk))
})

test_that("package_deliverables creates a zip archive with data deliverables", {
  skip_if_not_installed("zip")

  tmp <- withr::local_tempdir()
  data_file <- file.path(tmp, "test_table.xlsx")
  file.create(data_file)
  zip_out <- file.path(tmp, "bundle.zip")

  res_zip <- package_deliverables(
    pipeline_objects = list(),
    data_deliverables = list(data_file),
    zip_path = zip_out
  )

  expect_true(file.exists(zip_out))
  expect_equal(normalizePath(res_zip), normalizePath(zip_out))
})

test_that("package_deliverables writes a relative zip_path into the working directory, not the staging folder", {
  skip_if_not_installed("zip")

  tmp <- withr::local_tempdir()
  data_file <- file.path(tmp, "t.csv")
  writeLines("a,b", data_file)
  withr::local_dir(tmp)

  # The archive used to be written inside the staging folder, which is deleted
  # afterwards: the call reported success and returned a path to a file that no
  # longer existed.
  res <- package_deliverables(list(), data_deliverables = list(data_file), zip_path = "rel.zip")
  expect_true(file.exists(file.path(tmp, "rel.zip")))
  expect_true(file.exists(res))
  expect_match(res, "^([A-Za-z]:)?/")   # absolute, as documented
  expect_equal(normalizePath(res), normalizePath(file.path(tmp, "rel.zip")))
  expect_equal(basename(zip::zip_list(res)$filename[!grepl("/$", zip::zip_list(res)$filename)]), "t.csv")
})

test_that("package_deliverables keeps deliverables that share a file name instead of overwriting", {
  skip_if_not_installed("zip")

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "a"))
  dir.create(file.path(tmp, "b"))
  writeLines("from a", file.path(tmp, "a", "results.xlsx"))
  writeLines("from b, longer", file.path(tmp, "b", "results.xlsx"))
  zip_out <- file.path(tmp, "bundle.zip")

  expect_warning(
    package_deliverables(
      list(),
      data_deliverables = list(file.path(tmp, "a", "results.xlsx"), file.path(tmp, "b", "results.xlsx")),
      zip_path = zip_out
    ),
    "share the file name 'results.xlsx'.*results_2.xlsx"
  )

  extracted <- withr::local_tempdir()
  zip::unzip(zip_out, exdir = extracted)
  files <- list.files(extracted, recursive = TRUE, full.names = TRUE)
  expect_setequal(basename(files), c("results.xlsx", "results_2.xlsx"))
  # both payloads survive (neither file was overwritten by the other)
  payloads <- vapply(files, function(f) readLines(f, warn = FALSE)[1], character(1))
  expect_setequal(unname(payloads), c("from a", "from b, longer"))
})

# Folders named stage_* that are in tempdir() right now.
stage_dirs <- function() list.files(tempdir(), pattern = "^stage_")

test_that("package_deliverables keeps a stage that climbs out of the staging folder inside it", {
  skip_if_not_installed("zip")
  tmp <- withr::local_tempdir()
  writeLines("pdf bytes", file.path(tmp, "r.pdf"))
  marker <- paste0("escape_marker_", as.integer(runif(1, 1e6, 1e7)))
  # Two levels up from <staging>/<stage> is the folder that holds tempdir().
  outside <- file.path(dirname(tempdir()), marker)
  withr::defer(unlink(outside, recursive = TRUE))
  obj <- FileOutputs("r", file.path(tmp, "r.qmd"), stage = paste0("../../", marker))
  zip_out <- file.path(tmp, "bundle.zip")

  suppressMessages(package_deliverables(list(obj), output_formats = "pdf", zip_path = zip_out))

  expect_false(dir.exists(outside))
  entries <- zip::zip_list(zip_out)$filename
  expect_true(paste0(marker, "/r.pdf") %in% entries)
})

test_that("package_deliverables counts only the files it really copied", {
  skip_if_not_installed("zip")
  tmp <- withr::local_tempdir()
  good <- file.path(tmp, "good.csv")
  writeLines("a,b", good)
  a_folder <- file.path(tmp, "a_folder")
  dir.create(a_folder)   # file.exists() is TRUE, but a folder cannot be copied as a file
  zip_out <- file.path(tmp, "bundle.zip")

  expect_warning(
    expect_message(
      package_deliverables(list(), data_deliverables = list(good, a_folder), zip_path = zip_out),
      "packaged 1 files"
    ),
    "Could not copy '.*a_folder'"
  )
  expect_equal(grep("good.csv", zip::zip_list(zip_out)$filename, value = TRUE), "00_Data_Deliverables/good.csv")
})

test_that("package_deliverables removes its staging folder when zipping fails", {
  skip_if_not_installed("zip")
  tmp <- withr::local_tempdir()
  good <- file.path(tmp, "good.csv")
  writeLines("a,b", good)
  before <- stage_dirs()
  Sys.sleep(1.1)   # the staging folder is named after the second: keep it from replacing an older one

  testthat::local_mocked_bindings(zip = function(...) stop("disk full"), .package = "zip")
  expect_error(package_deliverables(list(), data_deliverables = list(good), zip_path = file.path(tmp, "b.zip")), "disk full")
  expect_equal(setdiff(stage_dirs(), before), character(0))
})
