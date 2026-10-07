# Force the sequential code path: spinning up real `future`/`furrr` worker
# processes in a unit test is slow and, under `devtools::load_all()` (as
# opposed to an installed package), those workers can't find TempleCBE to
# attach it, which fails for reasons unrelated to search_wrappers.R itself.
local_sequential_search <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base",
    .env = .env
  )
}

test_that("read_search finds common data-import calls", {
  local_sequential_search()
  tmp_dir <- file.path(tempdir(), "test_read_search")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "df <- readRDS(here::here('data', 'df.rds'))",
    "wb <- read_workbook(here::here('data', 'wb.xlsx'))",
    "out <- write.csv(df, 'out.csv')"
  ), file.path(tmp_dir, "script1.R"))

  res <- read_search(tmp_dir)

  expect_true(all(c("file", "path", "line_number", "line", "pattern") %in% names(res)))
  expect_true("readRDS" %in% res$pattern)
  expect_true("read_workbook" %in% res$pattern)
  expect_false("write.csv" %in% res$pattern)
})

test_that("write_search finds common data-export calls", {
  local_sequential_search()
  tmp_dir <- file.path(tempdir(), "test_write_search")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "df <- readRDS(here::here('data', 'df.rds'))",
    "writexl::write_xlsx(df, 'out.xlsx')",
    "saveRDS(df, 'out.rds')"
  ), file.path(tmp_dir, "script1.R"))

  res <- write_search(tmp_dir)

  expect_true("writexl::write_xlsx" %in% res$pattern)
  expect_true("saveRDS" %in% res$pattern)
  expect_false("readRDS" %in% res$pattern)
})

test_that("write_search excludes comment-only lines by default", {
  local_sequential_search()
  tmp_dir <- file.path(tempdir(), "test_write_search_comments")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "# saveRDS(df, 'commented_out.rds')",
    "saveRDS(df, 'out.rds')"
  ), file.path(tmp_dir, "script1.R"))

  res <- write_search(tmp_dir)

  expect_equal(nrow(res), 1)
  expect_match(res$line[1], "out\\.rds")
})
