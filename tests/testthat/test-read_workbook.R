test_that("read_workbook reads every sheet into a named list of tibbles", {
  skip_if_not_installed("writexl")

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)

  writexl::write_xlsx(
    list(first = data.frame(a = 1:3), second = data.frame(b = letters[1:2])),
    tmp_xlsx
  )

  res <- read_workbook(tmp_xlsx)

  expect_type(res, "list")
  expect_named(res, c("first", "second"))
  expect_equal(res$first$a, 1:3)
  expect_equal(res$second$b, letters[1:2])
})
