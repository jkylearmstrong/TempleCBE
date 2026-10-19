test_that("write_xlsx is supported via deprecated wrapper", {
  expect_true(is.function(TempleCBE::write_xlsx))

  path <- withr::local_tempfile(fileext = ".xlsx")
  expect_warning(TempleCBE::write_xlsx(mtcars, path), "deprecated")
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
})
