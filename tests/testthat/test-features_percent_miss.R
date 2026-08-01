test_that("features_percent_miss accurately calculates missingness", {
  df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
  res <- features_percent_miss(df)
  
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_equal(res$SumNa[res$feature == "b"], 2)
  expect_equal(res$PctNa[res$feature == "b"], 0.5)
  expect_equal(res$PctNa[res$feature == "a"], 0.25)
})

test_that("plot.features_percent_miss returns a ggplot object", {
  df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
  res <- features_percent_miss(df)
  p <- plot(res)
  expect_s3_class(p, "ggplot")
})
