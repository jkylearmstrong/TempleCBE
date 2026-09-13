test_that("normalize_safely turns blanks into NA and never fails", {
  out <- normalize_safely(c("", NA, "a/../b.txt"))
  expect_length(out, 3)
  expect_true(all(is.na(out[1:2])))
  expect_equal(unname(out[3]), normalizePath("a/../b.txt", winslash = "\\", mustWork = FALSE))
})

test_that("parse_here_call_vec resolves quoted here::here() arguments", {
  skip_if_not_installed("here")
  x <- c(
    "p <- here::here('a','b','file.xlsx')",
    'q <- here::here("x y","z","k.xlsx")',
    "no call here"
  )
  out <- parse_here_call_vec(x)
  expect_length(out, 3)
  expect_equal(out[1], as.character(here::here("a", "b", "file.xlsx")))
  expect_match(out[2], "x y.*z.*k.xlsx")
  expect_true(is.na(out[3]))
})

test_that("file_meta_fs returns one row per file, and a typed empty tibble for none", {
  empty <- file_meta_fs(character(0))
  expect_equal(nrow(empty), 0)
  expect_named(empty, c("path", "file_name", "dir_name", "dir_path", "m_time", "c_time", "size", "uname"))

  f <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", f)
  meta <- file_meta_fs(f)
  expect_equal(nrow(meta), 1)
  expect_equal(meta$file_name, basename(f))
  expect_equal(meta$dir_name, basename(dirname(f)))
  expect_gt(meta$size, 0)
})

test_that("extract_win_posix_paths splits Windows and POSIX .xlsx paths", {
  x <- c("C:\\data\\r\\table_1.xlsx", "/home/kyle/a.b/c/table-2.xlsx", "none")
  df <- extract_win_posix_paths(x)
  expect_equal(df$file, c("table_1.xlsx", "table-2.xlsx", NA))
  expect_equal(df$dir[1], "C:\\data\\r")
  expect_equal(df$full_path[2], "/home/kyle/a.b/c/table-2.xlsx")
  expect_true(is.na(df$full_path[3]))
})

test_that("extract_all_xlsx_tokens returns every file name per string", {
  tokens <- extract_all_xlsx_tokens(c("read_excel('out/a.xlsx')", "none"))
  expect_equal(tokens[[1]], "a.xlsx")
  expect_length(tokens[[2]], 0)
})
