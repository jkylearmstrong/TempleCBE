test_that("validate_executable verifies existence and fails with structured error", {
  # Known existing executable (Rscript)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  expect_identical(validate_executable(rscript), rscript)

  # Non-existent executable throws external_process_error
  expect_error(
    validate_executable("non_existent_binary_9999_xyz"),
    class = "external_process_error"
  )

  # Structured error attributes
  err <- tryCatch(
    validate_executable("non_existent_binary_9999_xyz"),
    external_process_error = function(e) e
  )
  expect_s3_class(err, "external_process_error")
  expect_identical(err$failure_mode, "missing_executable")
  expect_true(is.na(err$exit_code))
  expect_identical(err$cmd, "non_existent_binary_9999_xyz")

  # Invalid argument validation
  expect_error(validate_executable(""), class = "external_process_error")
  expect_error(validate_executable(character(0)), class = "external_process_error")
})

test_that("safe_system2 executes valid commands and handles exit codes", {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  # Successful command
  out <- safe_system2(rscript, c("-e", shQuote("cat('hello')")))
  expect_true(any(trimws(out) == "hello"))
  expect_null(attr(out, "status"))

  # Command with non-zero exit code and check = TRUE raises external_process_error
  expect_error(
    safe_system2(rscript, c("-e", shQuote("quit(status = 42)")), check = TRUE, log_failures = FALSE),
    class = "external_process_error"
  )

  # Validate structured fields on error
  err <- tryCatch(
    safe_system2(rscript, c("-e", shQuote("cat('fatal failure', file = stderr()); quit(status = 42)")), check = TRUE, log_failures = FALSE),
    external_process_error = function(e) e
  )
  expect_s3_class(err, "external_process_error")
  expect_identical(err$exit_code, 42L)
  expect_identical(err$failure_mode, "non_zero_exit")
  expect_true(grepl("fatal failure", err$stderr))

  # With check = FALSE, returns status instead of raising error
  res_no_check <- safe_system2(rscript, c("-e", shQuote("quit(status = 42)")), check = FALSE, log_failures = FALSE)
  expect_equal(attr(res_no_check, "status"), 42L)
})

test_that("safe_system2 produces actionable logs on failure", {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  expect_message(
    tryCatch(
      safe_system2(rscript, c("-e", shQuote("quit(status = 7)")), check = TRUE, log_failures = TRUE),
      error = function(e) NULL
    ),
    "\\[EXTERNAL_PROCESS_FAIL\\] Command '.*' exited with code 7"
  )
})

test_that("safe_shell executes commands or fails with structured error", {
  # Simple echo command
  out <- safe_shell("echo hello_shell", check = TRUE, log_failures = FALSE)
  expect_true(any(grepl("hello_shell", out)))

  # Failing shell command with check = TRUE
  fail_cmd <- if (.Platform$OS.type == "windows") "exit 5" else "exit 5"
  expect_error(
    safe_shell(fail_cmd, check = TRUE, log_failures = FALSE),
    class = "external_process_error"
  )
})
