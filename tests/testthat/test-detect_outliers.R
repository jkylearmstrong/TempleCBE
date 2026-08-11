test_that("detect_outliers identifies IQR fence outliers", {
  x <- c(1, 2, 3, 4, 100)
  fences <- calculate_fences(x)
  expect_true(fences$upper_inner_fence < 100)

  # .outlier/.outlier_type are factors (matches the original implementation)
  flags <- flag_outliers(x)
  expect_equal(as.character(flags$.outlier[5]), "TRUE")
  expect_equal(as.character(flags$.outlier[1]), "FALSE")
  expect_equal(as.character(flags$.outlier_type[5]), "EXTREME")
  expect_equal(as.character(flags$.outlier_type[1]), "NONE")

  df <- data.frame(a = c(1, 2, 3, 4, 100))
  res <- detect_outliers(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$column, "a")
  expect_equal(res$value, 100)

  res_all <- detect_outliers(df, outliers_only = FALSE)
  expect_equal(nrow(res_all), 5)

  # Test numeric vector input directly
  res_vec <- detect_outliers(x)
  expect_equal(nrow(res_vec), 1)
  expect_equal(res_vec$value, 100)
})
