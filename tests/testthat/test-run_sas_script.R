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

test_that("run_sas_script refuses a missing program or executable", {
  expect_error(run_sas_script("no_such_program.sas", sas_path = "sas"), "existing .sas file")

  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  expect_error(run_sas_script(prog, sas_path = NULL), "No SAS executable found")
  expect_error(run_sas_script(prog, sas_path = "non_existent_sas_binary"), class = "external_process_error")
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
