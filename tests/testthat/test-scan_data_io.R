# See test-search_wrappers.R for why the parallel `future`/`furrr` path is
# forced off in tests: it's slow and unreliable in a dev/CI sandbox.
local_sequential_search <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base",
    .env = .env
  )
}

test_that("scan_data_io classifies write outputs, read inputs, and unknown files", {
  local_sequential_search()
  tmp_dir <- file.path(tempdir(), "test_scan_data_io")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data_dir <- file.path(tmp_dir, "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  # A file the code writes and that already exists on disk.
  writeLines("x", file.path(data_dir, "written.xlsx"))
  # A file the code reads and that already exists on disk.
  writeLines("x", file.path(data_dir, "input.xlsx"))
  # A file present on disk with no matching read/write call anywhere.
  writeLines("x", file.path(data_dir, "orphan.xlsx"))

  script <- file.path(tmp_dir, "script.R")
  writeLines(c(
    sprintf("writexl::write_xlsx(df, '%s')", file.path(data_dir, "written.xlsx")),
    sprintf("df <- readxl::read_excel('%s')", file.path(data_dir, "input.xlsx")),
    sprintf("saveRDS(df, '%s')", file.path(tmp_dir, "not_xlsx.rds"))
  ), script)

  res <- scan_data_io(tmp_dir, project_root = tmp_dir, ext = "xlsx")

  expect_true(all(c("writes", "inputs", "missing_write_dirs", "files") %in% names(res)))

  files <- res$files
  expect_equal(nrow(files), 3)

  written_row <- files[files$file_name == "written.xlsx", ]
  expect_equal(written_row$file_class, "write_output")

  input_row <- files[files$file_name == "input.xlsx", ]
  expect_equal(input_row$file_class, "workflow_input")

  orphan_row <- files[files$file_name == "orphan.xlsx", ]
  expect_equal(orphan_row$file_class, "unknown")
})

test_that("scan_data_io flags a write target that doesn't exist yet as a missing deliverable", {
  local_sequential_search()
  tmp_dir <- file.path(tempdir(), "test_scan_data_io_missing")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data_dir <- file.path(tmp_dir, "deliverables")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  script <- file.path(tmp_dir, "script.R")
  writeLines(
    sprintf("writexl::write_xlsx(df, '%s')", file.path(data_dir, "not_yet_generated.xlsx")),
    script
  )

  res <- scan_data_io(tmp_dir, project_root = tmp_dir, ext = "xlsx")

  expect_equal(nrow(res$missing_write_dirs), 1)
  expect_equal(basename(res$missing_write_dirs$dir_path), "deliverables")
})
