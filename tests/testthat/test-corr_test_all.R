test_that("corr_test_all computes pairwise correlations and p-values", {
  res <- corr_test_all(mtcars[, c("mpg", "hp", "wt", "qsec")])
  
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("var1", "var2", "r", "p_value") %in% names(res)))
  expect_equal(nrow(res), choose(4, 2))
  expect_true(all(res$r >= -1 & res$r <= 1))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))

  # Test error handling when insufficient numeric columns
  expect_error(corr_test_all(data.frame(a = 1:5)), "at least 2 numeric columns")
})
