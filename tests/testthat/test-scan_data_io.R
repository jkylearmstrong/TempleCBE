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

test_that("scan_data_io resolves paths containing repeated separators", {
  # macOS sets TMPDIR with a trailing slash, so tempdir() there looks like
  # `.../T//RtmpXXXX`. A doubled separator must not truncate the parsed path.
  local_sequential_search()
  tmp_dir <- paste0(tempdir(), "//test_scan_data_io_double_sep")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data_dir <- file.path(tmp_dir, "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("x", file.path(data_dir, "written.xlsx"))
  writeLines("x", file.path(data_dir, "input.xlsx"))

  writeLines(c(
    sprintf("writexl::write_xlsx(df, '%s')", file.path(data_dir, "written.xlsx")),
    sprintf("df <- readxl::read_excel('%s')", file.path(data_dir, "input.xlsx"))
  ), file.path(tmp_dir, "script.R"))

  res <- scan_data_io(tmp_dir, project_root = tmp_dir, ext = "xlsx")
  files <- res$files

  expect_equal(files$file_class[files$file_name == "written.xlsx"], "write_output")
  expect_equal(files$file_class[files$file_name == "input.xlsx"], "workflow_input")
  expect_equal(nrow(res$missing_write_dirs), 0)
})

test_that("scan_data_io applies max_depth with a relative project_root", {
  local_sequential_search()
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "a", "b"), recursive = TRUE)
  writeLines("x", file.path(tmp_dir, "top.xlsx"))
  writeLines("x", file.path(tmp_dir, "a", "b", "deep.xlsx"))
  writeLines(
    sprintf("writexl::write_xlsx(df, '%s')", normalizePath(file.path(tmp_dir, "top.xlsx"), winslash = "/")),
    file.path(tmp_dir, "script.R")
  )
  withr::local_dir(dirname(tmp_dir))

  shallow <- scan_data_io(basename(tmp_dir), max_depth = 1)
  expect_setequal(unique(shallow$files$file_name), "top.xlsx")

  everything <- scan_data_io(basename(tmp_dir))
  expect_setequal(unique(everything$files$file_name), c("top.xlsx", "deep.xlsx"))
})

test_that("scan_data_io tokenizes relative and bare paths without code text (A4-06)", {
  local_sequential_search()
  tmp_dir <- withr::local_tempdir()
  scripts_dir <- file.path(tmp_dir, "scripts")
  dir.create(scripts_dir, recursive = TRUE)

  for (f in c("in.xlsx", "out.xlsx", "orphan.xlsx")) {
    writeLines("x", file.path(tmp_dir, f))
  }
  writeLines(
    c("df <- readxl::read_excel('in.xlsx')", "writexl::write_xlsx(df, 'out.xlsx')"),
    file.path(scripts_dir, "s.R")
  )

  res <- scan_data_io(scripts_dir, project_root = tmp_dir, ext = "xlsx")

  files <- res$files
  expect_equal(files$file_class[files$file_name == "in.xlsx"], "workflow_input")
  expect_equal(files$file_class[files$file_name == "out.xlsx"], "write_output")
  expect_equal(files$file_class[files$file_name == "orphan.xlsx"], "unknown")

  # Resolved read path must be the clean file path, not containing surrounding code text
  expect_false(any(grepl("read_excel", res$inputs$infile_path)))
  expect_true(any(grepl("in\\.xlsx$", res$inputs$infile_path)))
})

test_that("scan_data_io runs cleanly on reads-only and writes-only scripts (A4-07)", {
  local_sequential_search()
  tmp_dir <- withr::local_tempdir()
  scripts_dir <- file.path(tmp_dir, "scripts")
  dir.create(scripts_dir, recursive = TRUE)

  writeLines("x", file.path(tmp_dir, "in.xlsx"))
  writeLines("df <- readxl::read_excel('in.xlsx')", file.path(scripts_dir, "read_only.R"))

  # Must not error with dplyr/vctrs type mismatch when writes is empty
  expect_no_error(res_read <- scan_data_io(scripts_dir, project_root = tmp_dir, ext = "xlsx"))
  expect_equal(res_read$files$file_class[res_read$files$file_name == "in.xlsx"], "workflow_input")
  expect_equal(nrow(res_read$writes), 0)

  # Writes-only script
  unlink(file.path(scripts_dir, "read_only.R"))
  writeLines("writexl::write_xlsx(df, 'in.xlsx')", file.path(scripts_dir, "write_only.R"))
  expect_no_error(res_write <- scan_data_io(scripts_dir, project_root = tmp_dir, ext = "xlsx"))
  expect_equal(res_write$files$file_class[res_write$files$file_name == "in.xlsx"], "write_output")
  expect_equal(nrow(res_write$inputs), 0)
})

test_that("scan_data_io validates ext and protects against false heuristic matches (A4-21)", {
  local_sequential_search()
  tmp_dir <- withr::local_tempdir()
  scripts_dir <- file.path(tmp_dir, "scripts")
  dir.create(scripts_dir, recursive = TRUE)

  # ext validation
  expect_error(scan_data_io(scripts_dir, project_root = tmp_dir, ext = "csv|xlsx"), "single alphanumeric file extension")
  expect_error(scan_data_io(scripts_dir, project_root = tmp_dir, ext = 123), "single non-NA character string")

  # leading dot is stripped cleanly
  writeLines("x", file.path(tmp_dir, "test.csv"))
  writeLines("d <- read.csv('test.csv')", file.path(scripts_dir, "s.R"))
  expect_no_error(r_dot <- scan_data_io(scripts_dir, project_root = tmp_dir, ext = ".csv"))
  expect_equal(r_dot$files$file_class[r_dot$files$file_name == "test.csv"], "workflow_input")

  # Heuristic paths$a must NOT match banana_report.xlsx because key 'a' is too short (< 3 chars)
  writeLines("x", file.path(tmp_dir, "banana_report.xlsx"))
  writeLines("x <- read_workbook(paths$a)", file.path(scripts_dir, "heur.R"))
  r_heur <- scan_data_io(scripts_dir, project_root = tmp_dir, ext = "xlsx")
  expect_equal(r_heur$files$file_class[r_heur$files$file_name == "banana_report.xlsx"], "unknown")
})

test_that("scan_data_io confines missing_write_dirs to project_root (A4-22)", {
  local_sequential_search()
  tmp_dir <- withr::local_tempdir()
  proj <- file.path(tmp_dir, "proj")
  outside <- file.path(tmp_dir, "outside")
  dir.create(file.path(proj, "scripts"), recursive = TRUE)
  dir.create(outside, recursive = TRUE)

  writeLines("x", file.path(outside, "unrelated_private.xlsx"))
  # Script points write to an outside uncreated file
  outside_target <- file.path(outside, "not_yet_written.xlsx")
  writeLines(
    sprintf("writexl::write_xlsx(df, '%s')", outside_target),
    file.path(proj, "scripts", "s.R")
  )

  res <- scan_data_io(file.path(proj, "scripts"), project_root = proj, ext = "xlsx")

  # outside folder must not appear in missing_write_dirs deliverables
  expect_type(res, "list")
  expect_false(any(grepl("outside", res$missing_write_dirs$dir_path)))
})
