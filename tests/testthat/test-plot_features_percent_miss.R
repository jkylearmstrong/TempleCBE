test_that("plot_features_percent_miss and S3 plot method return ggplot objects", {
  df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
  
  p1 <- plot_features_percent_miss(df)
  expect_s3_class(p1, "ggplot")

  # Test top_n parameter
  p2 <- plot_features_percent_miss(df, top_n = 1)
  expect_s3_class(p2, "ggplot")

  # Test S3 plot method on features_percent_miss object
  res <- features_percent_miss(df)
  p3 <- plot(res)
  expect_s3_class(p3, "ggplot")
})
