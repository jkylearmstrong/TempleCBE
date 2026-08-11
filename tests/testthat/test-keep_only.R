test_that("keep_only removes every object except the ones named", {
  e <- new.env()
  local({
    a <- 1
    b <- 2
    c <- 3
    keep_only("a", .dontask = TRUE)
  }, envir = e)

  expect_equal(ls(e), "a")
})

test_that("keep_only does nothing if every object is already in the keep list", {
  e <- new.env()
  local({
    a <- 1
    b <- 2
    keep_only(c("a", "b"), .dontask = TRUE)
  }, envir = e)

  expect_setequal(ls(e), c("a", "b"))
})

test_that("delete_nul_files errors clearly on non-Windows platforms", {
  skip_on_os("windows")
  expect_error(delete_nul_files(path = tempdir()), "Windows")
})

test_that("nul_delete_commands quotes paths for cmd.exe instead of hand-splicing quotes", {
  cmds <- nul_delete_commands("C:/some/dir/nul")
  expect_equal(cmds, 'del "\\\\.\\C:\\some\\dir\\nul"')

  # A path containing a literal double quote (not realistically producible
  # on Windows, since '"' is an invalid filename character there, but this
  # confirms shQuote() -- not manual string splicing -- is doing the escaping)
  cmds2 <- nul_delete_commands('C:/weird"dir/nul')
  expect_match(cmds2, "^del ")
  expect_true(nchar(cmds2) > nchar("del "))
})

test_that("delete_nul_files reports no files found in a directory with none", {
  skip_on_os(c("mac", "linux", "solaris"))
  empty_dir <- file.path(tempdir(), "no_nul_here")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_message(res <- delete_nul_files(path = empty_dir, .dontask = TRUE), "No stray")
  expect_equal(res, character(0))
})
