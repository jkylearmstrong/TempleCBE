test_that("sd.error calculates standard error accurately", {
  x <- c(1, 2, 3, 4, 5)
  se <- sd.error(x)
  expect_equal(se, sd(x) / sqrt(5))
})

test_that("my_summary_table generates expected summary statistics", {
  df <- data.frame(cyl = c(4, 4, 6, 6), mpg = c(20, 22, 18, 16))
  res <- dplyr::group_by(df, cyl) |> my_summary_table(mpg)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true("variable" %in% names(res))
})
