test_that("write_xlsx is re-exported from writexl", {
  expect_identical(TempleCBE::write_xlsx, writexl::write_xlsx)

  path <- withr::local_tempfile(fileext = ".xlsx")
  TempleCBE::write_xlsx(mtcars, path)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
})
