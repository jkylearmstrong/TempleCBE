test_that("create_report `t_test_example` works", {
  tempy <- withr::local_tempdir("create_report_t_test_example")

  create_report(tempy)

  missing_files <- c("bib.bib", "t_test_child.qmd", "t_test_example.qmd", "title.tex") |>
    setdiff(list.files(tempy))

  expect_true(length(missing_files) == 0, info = paste("Missing files:", paste(missing_files, collapse = ", ")))
})


test_that("create_report `example` works", {
  tempy1 <- withr::local_tempdir("create_report_example")

  template_name_ex = 'example'
  child_name <- paste0(template_name_ex, "_child")

  create_report(tempy1,  template_name = template_name_ex)

  missing_files <- c("bib.bib", paste0(template_name_ex,'.qmd'), "title.tex") |>
    setdiff(list.files(tempy1))

  expect_true(length(missing_files) == 0, info = paste("Missing files:", paste(missing_files, collapse = ", ")))
})


test_that("create_report `temple` copies the branded template without title.tex or child", {
  loc <- withr::local_tempdir("create_report_temple")

  expect_message(res <- create_report(loc, template_name = "temple"), "use_temple_brand")

  expect_true(res$template_created)
  expect_true(res$bib_created)
  expect_false(res$title_tex_created)
  expect_false(res$child_created)
  expect_setequal(list.files(loc), c("temple.qmd", "bib.bib"))
})

test_that("create_report `temple` is Quarto-only", {
  loc <- withr::local_tempdir("create_report_temple_rmd")
  expect_warning(suppressMessages(create_report(loc, template_name = "temple", type = ".Rmd")), "Quarto-only")
  expect_true(file.exists(file.path(loc, "temple.qmd")))
})

test_that("create_report `filename` lets two reports share one `location`", {
  loc <- withr::local_tempdir("create_report_two_reports")

  suppressMessages(create_report(loc, template_name = "temple", filename = "analysis1"))
  suppressMessages(create_report(loc, template_name = "temple", filename = "analysis2"))

  expect_true(file.exists(file.path(loc, "analysis1.qmd")))
  expect_true(file.exists(file.path(loc, "analysis2.qmd")))
  expect_false(file.exists(file.path(loc, "temple.qmd")))
})

test_that("create_report without `filename` overwrites a same-named report and warns", {
  loc <- withr::local_tempdir("create_report_overwrite")

  suppressMessages(create_report(loc, template_name = "temple"))
  expect_warning(
    suppressMessages(create_report(loc, template_name = "temple")),
    "Overwriting existing report file"
  )
})

test_that("create_report `filename` strips a redundant extension", {
  loc <- withr::local_tempdir("create_report_filename_ext")

  suppressMessages(create_report(loc, template_name = "temple", filename = "analysis1.qmd"))

  expect_true(file.exists(file.path(loc, "analysis1.qmd")))
  expect_false(file.exists(file.path(loc, "analysis1.qmd.qmd")))
})

test_that("create_report rejects an empty `filename`", {
  loc <- withr::local_tempdir("create_report_bad_filename")
  expect_error(create_report(loc, filename = "  "), "`filename`")
})

test_that("create_report `eda_tables` copies report and child template", {
  loc <- withr::local_tempdir("create_report_eda_tables")
  res <- create_report(loc, template_name = "eda_tables", child = TRUE)

  expect_true(res$template_created)
  expect_true(res$child_created)
  expect_true(file.exists(file.path(loc, "eda_tables.qmd")))
  expect_true(file.exists(file.path(loc, "child_eda_chi_square.qmd")))
})

