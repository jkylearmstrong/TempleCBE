test_that("sas_args writes the log and listing beside the program's stem", {
  args <- sas_args("dir/prog.SAS", "logs", "list")
  expect_equal(args, c(
    shQuote("dir/prog.SAS"),
    "-log", shQuote(file.path("logs", "prog.log")),
    "-print", shQuote(file.path("list", "prog.lst"))
  ))
})

test_that("find_sas prefers the templecbe.sas option", {
  exe <- withr::local_tempfile(fileext = ".exe")
  file.create(exe)
  withr::local_options(templecbe.sas = exe)
  expect_equal(find_sas(), exe)
})

test_that("find_sas recognizes SAS_EXE and SASROOT environment variables", {
  exe <- withr::local_tempfile(fileext = ".exe")
  file.create(exe)
  withr::local_options(templecbe.sas = NULL)
  
  withr::with_envvar(c(SAS_EXE = exe), {
    expect_equal(find_sas(), exe)
  })

  sasdir <- withr::local_tempdir()
  root_exe <- file.path(sasdir, if (.Platform$OS.type == "windows") "sas.exe" else "sas")
  file.create(root_exe)
  withr::with_envvar(c(SAS_EXE = "", SASROOT = sasdir), {
    expect_equal(find_sas(), root_exe)
  })
})

test_that("sas_available is TRUE exactly when find_sas finds an executable", {
  local_mocked_bindings(find_sas = function() NULL)
  expect_false(sas_available())
  local_mocked_bindings(find_sas = function() "path/to/sas")
  expect_true(sas_available())
})

test_that("run_sas_script refuses a missing program or executable", {
  expect_error(run_sas_script("no_such_program.sas", sas_path = "sas"), "existing .sas file")

  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  expect_error(run_sas_script(prog, sas_path = NULL), "No SAS executable found")
  expect_error(run_sas_script(prog, sas_path = "non_existent_sas_binary"), class = "external_process_error")
})

# A stand-in for the SAS executable that takes `secs` seconds and exits with
# `exit` (a real executable, so the whole process path runs).
fake_sas <- function(secs = 0, exit = 0L, envir = parent.frame()) {
  windows <- .Platform$OS.type == "windows"
  path <- withr::local_tempfile(fileext = if (windows) ".cmd" else ".sh", .local_envir = envir)
  writeLines(
    if (windows) {
      c("@echo off", if (secs > 0) paste0("ping -n ", ceiling(secs) + 1, " 127.0.0.1 >nul"), paste0("exit /b ", exit))
    } else {
      # exec: the shell is replaced by sleep, so stopping the process stops the wait
      c("#!/bin/sh", if (secs > 0) paste0("exec sleep ", secs), paste0("exit ", exit))
    },
    path
  )
  Sys.chmod(path, "0755")
  path
}

test_that("run_sas_script stops a run that outlasts its timeout, and says so", {
  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  logs <- withr::local_tempdir()

  t0 <- Sys.time()
  expect_error(
    run_sas_script(prog, sas_path = fake_sas(secs = 5), log_dir = logs, list_dir = logs, timeout = 1),
    "SAS did not finish within 1 seconds and was stopped. The log, as far as it got, is at: "
  )
  # Without a limit this would have run to the end of the 5 s.
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 4)
})

test_that("run_sas_script keeps its exit-status behaviour with and without a timeout", {
  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  logs <- withr::local_tempdir()
  run <- function(...) run_sas_script(prog, log_dir = logs, list_dir = logs, ...)

  # No limit by default; a limit that is not reached changes nothing.
  expect_equal(run(sas_path = fake_sas(exit = 0L)), 0L)
  expect_equal(run(sas_path = fake_sas(exit = 0L), timeout = 60), 0L)
  expect_warning(expect_equal(run(sas_path = fake_sas(exit = 1L), timeout = 60), 1L), "completed with warnings")
  expect_error(run(sas_path = fake_sas(exit = 2L), timeout = 60), "failed with exit code 2")
})

test_that("run_sas_script validates its timeout", {
  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  for (bad in list(-1, NA_real_, "slow", c(1, 2), NULL)) {
    expect_error(run_sas_script(prog, sas_path = "sas", timeout = bad), "`timeout` must be a single number of seconds")
  }
})

test_that("cbe_sas_macro_dir returns existing macro folder", {
  dir <- cbe_sas_macro_dir()
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, "cbe_macros.sas")))
})

test_that("cbe_sas_macro_path locates bundled macros and errors gracefully on unknown", {
  path_all <- cbe_sas_macro_path("cbe_macros")
  expect_true(file.exists(path_all))
  expect_match(path_all, "cbe_macros\\.sas$")

  path_brier <- cbe_sas_macro_path("cbe_brier_score.sas")
  expect_true(file.exists(path_brier))

  path_cox <- cbe_sas_macro_path("cbe_cox_phreg.sas")
  expect_true(file.exists(path_cox))

  path_cp <- cbe_sas_macro_path("cbe_counting_process.sas")
  expect_true(file.exists(path_cp))

  path_coxtvc <- cbe_sas_macro_path("coxtvc.sas")
  expect_true(file.exists(path_coxtvc))

  path_cpdata <- cbe_sas_macro_path("cpdata.sas")
  expect_true(file.exists(path_cpdata))

  expect_error(cbe_sas_macro_path("non_existent_macro.sas"), "was not found")
})

test_that("SAS macros use explicit blank delimiter for countw/scan to handle decimal times", {
  cp_lines <- readLines(cbe_sas_macro_path("cbe_counting_process.sas"), warn = FALSE)
  countw_cp <- grep("countw\\s*\\(", cp_lines, value = TRUE)
  expect_true(length(countw_cp) > 0)
  expect_true(all(grepl("%str\\(\\s*\\)", countw_cp)))

  bs_lines <- readLines(cbe_sas_macro_path("cbe_brier_score.sas"), warn = FALSE)
  countw_bs <- grep("countw\\s*\\(", bs_lines, value = TRUE)
  expect_true(length(countw_bs) > 0)
  expect_true(all(grepl("%str\\(\\s*\\)", countw_bs)))

  scan_bs <- grep("%scan\\s*\\(", bs_lines, value = TRUE)
  expect_true(length(scan_bs) > 0)
  expect_true(all(grepl("%str\\(\\s*\\)", scan_bs)))
})
