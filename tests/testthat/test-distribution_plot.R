test_that("distribution_plot returns a ggplot object for each normalization method", {
  df <- data.frame(a = rnorm(100, -10, 0.5), b = rpois(100, 5))

  expect_s3_class(distribution_plot(df), "ggplot")
  expect_s3_class(distribution_plot(df, method = "min_max"), "ggplot")
  expect_s3_class(distribution_plot(df, method = "z"), "ggplot")
  expect_s3_class(distribution_plot(df, method = "none"), "ggplot")
})

test_that("distribution_plot errors on an unrecognized method", {
  df <- data.frame(a = 1:10, b = 10:1)
  expect_error(distribution_plot(df, method = "bogus"), "must be one of")
})

test_that("distribution_plot drops non-numeric columns before plotting", {
  df <- data.frame(a = 1:10, label = letters[1:10])
  p <- distribution_plot(df)
  expect_false("label" %in% unique(as.character(p$data$name)))
})
