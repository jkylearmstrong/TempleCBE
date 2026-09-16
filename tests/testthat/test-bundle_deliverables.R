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
