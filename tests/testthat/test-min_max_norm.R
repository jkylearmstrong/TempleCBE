test_that("min_max_norm correctly scales numeric data", {
  x <- c(10, 20, 30, 40, 50)
  norm_x <- min_max_norm(x)
  expect_equal(min(norm_x), 0)
  expect_equal(max(norm_x), 1)
  expect_equal(norm_x[3], 0.5)
  
  # non-numeric columns are dropped by design (matches the original
  # datasci implementation this was migrated from) -- range_norm() is the
  # variant that keeps them
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
  norm_df <- min_max_norm(df)
  expect_equal(norm_df$a, c(0, 0.5, 1))
  expect_null(norm_df$b)

  norm_df2 <- range_norm(df)
  expect_equal(norm_df2$b, c("x", "y", "z"))
})
