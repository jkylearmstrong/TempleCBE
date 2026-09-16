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

test_that("run_sas_script refuses a missing program or executable", {
  expect_error(run_sas_script("no_such_program.sas", sas_path = "sas"), "existing .sas file")

  prog <- withr::local_tempfile(fileext = ".sas")
  writeLines("data _null_; run;", prog)
  expect_error(run_sas_script(prog, sas_path = NULL), "No SAS executable found")
  expect_error(run_sas_script(prog, sas_path = "non_existent_sas_binary"), class = "external_process_error")
})
