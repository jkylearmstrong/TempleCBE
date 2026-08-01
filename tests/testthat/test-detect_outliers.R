test_that("detect_outliers identifies IQR fence outliers", {
  x <- c(1, 2, 3, 4, 100)
  fences <- calculate_fences(x)
  flags <- flag_outliers(x)
  expect_true(flags[5])
  expect_false(flags[1])
  
  df <- data.frame(a = c(1, 2, 3, 4, 100))
  res <- detect_outliers(df)
  expect_equal(res$n_outliers, 1)
})
