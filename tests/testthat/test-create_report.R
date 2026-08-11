test_that("create_report `t_test_example` works", {
  tempy <- tempdir()

  create_report(tempy)

  missing_files <- c("bib.bib", "t_test_child.qmd", "t_test_example.qmd", "title.tex") |>
    setdiff(list.files(tempy))

  expect_true(length(missing_files) == 0, info = paste("Missing files:", paste(missing_files, collapse = ", ")))
})


test_that("create_report `example` works", {
  tempy1 <- tempdir()

  template_name_ex = 'example'
  child_name <- paste0(template_name_ex, "_child")

  create_report(tempy1,  template_name = template_name_ex)

  missing_files <- c("bib.bib", paste0(template_name_ex,'.qmd'), "title.tex") |>
    setdiff(list.files(tempy1))

  expect_true(length(missing_files) == 0, info = paste("Missing files:", paste(missing_files, collapse = ", ")))
})
