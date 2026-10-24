test_that("write_xlsx_file() writes a workbook, and the deprecated write_xlsx() is gone", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx_file(mtcars, path)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
  expect_false("write_xlsx" %in% getNamespaceExports("TempleCBE"))
})
