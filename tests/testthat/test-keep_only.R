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

test_that("keep_only stops before removing anything when a kept name is not there", {
  # keep_only("reslt") used to remove every object, "result" included.
  e <- new.env()
  expect_error(
    local({ result <- 1; big <- 2; keep_only("reslt", .dontask = TRUE) }, envir = e),
    "Nothing was removed: 'reslt' is not in the calling environment"
  )
  expect_setequal(ls(e), c("result", "big"))

  # One good name among misspelt ones does not save the call.
  e <- new.env()
  expect_error(
    local({ result <- 1; big <- 2; keep_only(c("result", "bigg", "bg"), .dontask = TRUE) }, envir = e),
    "'bigg', 'bg' are not in the calling environment"
  )
  expect_setequal(ls(e), c("result", "big"))

  # Only the calling environment counts: `pi` is in base, not here.
  e <- new.env()
  expect_error(local({ a <- 1; keep_only("pi", .dontask = TRUE) }, envir = e), "'pi' is not in the calling environment")
  expect_equal(ls(e), "a")
})

test_that("keep_only wants quoted names", {
  e <- new.env()
  # Unquoted, c(result, big) is the two numbers 1 and 2.
  expect_error(
    local({ result <- 1; big <- 2; keep_only(c(result, big), .dontask = TRUE) }, envir = e),
    "must be a character vector of object names"
  )
  expect_setequal(ls(e), c("result", "big"))

  # An unquoted name that is not an object is R's own error, and nothing is removed.
  expect_error(local({ a <- 1; keep_only(no_such_object, .dontask = TRUE) }, envir = e), "no_such_object")
  expect_error(local({ a <- 1; keep_only(NULL, .dontask = TRUE) }, envir = e), "must be a character vector")
  expect_error(local({ a <- 1; keep_only(NA_character_, .dontask = TRUE) }, envir = e), "must be a character vector")
  expect_setequal(ls(e), c("result", "big", "a"))
})

test_that("keep_only keeps names that start with a dot, and removes nothing it was not asked to", {
  e <- new.env()
  local({
    .hidden <- 1
    a <- 1
    b <- 2
    keep_only(c(".hidden", "a"), .dontask = TRUE)
  }, envir = e)
  expect_equal(ls(e, all.names = TRUE), c(".hidden", "a"))
})

test_that("inside a function keep_only works on that function's environment, arguments included", {
  # Documented behaviour: the function's own frame is the calling environment.
  f <- function(x) {
    y <- 1
    keep_only("y", .dontask = TRUE)
    exists("x", inherits = FALSE)
  }
  expect_false(f(3))

  g <- function(x) {
    y <- 1
    keep_only(c("x", "y"), .dontask = TRUE)
    exists("x", inherits = FALSE)
  }
  expect_true(g(3))

  # A misspelt name inside a function stops the call and leaves the arguments alone.
  h <- function(x) {
    y <- 1
    tryCatch(keep_only("yy", .dontask = TRUE), error = function(e) NULL)
    exists("x", inherits = FALSE) && exists("y", inherits = FALSE)
  }
  expect_true(h(3))
})

test_that("delete_nul_files errors clearly on non-Windows platforms", {
  skip_on_os("windows")
  expect_error(delete_nul_files(path = tempdir()), "Windows")
})

test_that("nul_device_path spells a path the way Windows opens a file called nul", {
  expect_equal(nul_device_path("C:/some/dir/nul"), "\\\\.\\C:\\some\\dir\\nul")
  expect_equal(
    nul_device_path(c("C:/a b/nul", "D:/x/NUL")),
    c("\\\\.\\C:\\a b\\nul", "\\\\.\\D:\\x\\NUL")
  )
  # Percent signs, quotes and the like are just characters: nothing is quoted or escaped for a shell.
  expect_equal(nul_device_path("C:/x%TEMP%y/nul"), "\\\\.\\C:\\x%TEMP%y\\nul")
  # A network share has its own device-namespace form.
  expect_equal(nul_device_path("//server/share/d/nul"), "\\\\.\\UNC\\server\\share\\d\\nul")
  expect_equal(nul_device_path(character(0)), character(0))
})

# Creates a file called "nul" in `dir` (Windows). A plain path cannot name it:
# it would be the NUL device, so it is created, and later removed, through its
# device path. The removal is deferred to the end of the calling test.
make_nul <- function(dir, name = "nul", .env = parent.frame()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- paste0("\\\\.\\", gsub("/", "\\\\", normalizePath(dir, winslash = "/")), "\\", name)
  stopifnot(file.create(path))
  withr::defer(suppressWarnings(file.remove(path)), envir = .env)
  path
}

# Is there a file called nul (in any case) in `dir`?
has_nul <- function(dir) any(tolower(list.files(dir, all.files = TRUE)) == "nul")

test_that("delete_nul_files deletes nul files whatever the folder is called, without handing cmd.exe a command", {
  skip_on_os(c("mac", "linux", "solaris"))
  root <- withr::local_tempdir()
  folders <- c("plain", "a&b c", "x%TEMP%y", "it's", "caret^name", "(paren)")
  for (f in folders) make_nul(file.path(root, f))
  make_nul(file.path(root, "plain", "deeper"), name = "NUL")   # nested, and in upper case

  res <- suppressMessages(delete_nul_files(root, .dontask = TRUE))

  # "x%TEMP%y" used to be expanded by cmd.exe, so the delete failed and the nul file stayed.
  expect_length(res, length(folders) + 1L)
  for (f in folders) expect_false(has_nul(file.path(root, f)), info = f)
  expect_false(has_nul(file.path(root, "plain", "deeper")))
})

test_that("delete_nul_files takes a relative path", {
  skip_on_os(c("mac", "linux", "solaris"))
  root <- withr::local_tempdir()
  make_nul(file.path(root, "sub"))
  withr::local_dir(root)

  res <- suppressMessages(delete_nul_files("sub", .dontask = TRUE))
  expect_length(res, 1L)
  expect_false(has_nul(file.path(root, "sub")))
})

test_that("delete_nul_files with .verify_command returns the paths and deletes nothing", {
  skip_on_os(c("mac", "linux", "solaris"))
  root <- withr::local_tempdir()
  make_nul(root)

  found <- suppressMessages(delete_nul_files(root, .dontask = TRUE, .verify_command = TRUE))
  expect_equal(found, nul_device_path(file.path(normalizePath(root, winslash = "/"), "nul")))
  expect_true(has_nul(root))
})

test_that("delete_nul_files does not follow a junction out of the folder it was given", {
  skip_on_os(c("mac", "linux", "solaris"))
  root <- withr::local_tempdir()
  inside <- file.path(root, "inside")
  outside <- file.path(root, "outside")
  dir.create(inside)
  make_nul(inside)
  make_nul(outside)
  link <- file.path(inside, "link")
  win <- function(p) gsub("/", "\\\\", p)
  system2("cmd.exe", c("/c", "mklink", "/J", shQuote(win(link)), shQuote(win(outside))), stdout = FALSE, stderr = FALSE)
  skip_if_not(dir.exists(link), "could not create a junction")
  # Removes the junction itself, not what it points to; runs before the folders go.
  withr::defer(system2("cmd.exe", c("/c", "rmdir", shQuote(win(link))), stdout = FALSE, stderr = FALSE))

  # list.files(recursive = TRUE) lists outside/nul as inside/link/nul.
  expect_message(
    found <- delete_nul_files(inside, .dontask = TRUE, .verify_command = TRUE),
    "reached through a link"
  )
  expect_length(found, 1L)
  expect_true(endsWith(found, "\\inside\\nul"))

  suppressMessages(delete_nul_files(inside, .dontask = TRUE))
  expect_false(has_nul(inside))
  expect_true(has_nul(outside))   # outside `path`: left alone
})

test_that("find_nul_files skips a folder that is a link to somewhere else (symbolic link)", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  inside <- file.path(root, "inside")
  outside <- file.path(root, "outside")
  dir.create(inside)
  dir.create(outside)
  writeLines("x", file.path(inside, "nul"))
  writeLines("x", file.path(outside, "NUL"))
  skip_if_not(file.symlink(outside, file.path(inside, "link")), "could not create a symbolic link")

  found <- suppressMessages(find_nul_files(inside))
  expect_equal(basename(found$full), "nul")
  expect_length(found$full, 1L)
  expect_equal(dirname(found$full), normalizePath(inside, winslash = "/"))
})

test_that("delete_nul_files names the files it could not delete", {
  skip_on_os(c("mac", "linux", "solaris"))
  root <- withr::local_tempdir()
  stuck <- make_nul(root)
  # Windows will not delete a read-only file.
  Sys.chmod(stuck, "0444")
  withr::defer(Sys.chmod(stuck, "0666"))

  expect_error(suppressMessages(delete_nul_files(root, .dontask = TRUE)), "Could not delete: .*nul")
  expect_true(has_nul(root))
})

test_that("delete_nul_files reports no files found in a directory with none", {
  skip_on_os(c("mac", "linux", "solaris"))
  empty_dir <- file.path(tempdir(), "no_nul_here")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_message(res <- delete_nul_files(path = empty_dir, .dontask = TRUE), "No stray")
  expect_equal(res, character(0))
})
