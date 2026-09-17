test_that("corr_test_all computes pairwise correlations and p-values", {
  res <- corr_test_all(mtcars[, c("mpg", "hp", "wt", "qsec")])

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("var1", "var2", "r", "p_value"))
  expect_equal(nrow(res), choose(4, 2))
  expect_true(all(res$r >= -1 & res$r <= 1))
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))

  # Test error handling when insufficient numeric columns
  expect_error(corr_test_all(data.frame(a = 1:5)), "at least 2 numeric columns")
})

test_that("corr_test_all pairs columns in column order", {
  res <- corr_test_all(mtcars[, c("wt", "mpg", "hp")], sort = "none")

  expect_equal(res$var1, c("wt", "wt", "mpg"))
  expect_equal(res$var2, c("mpg", "hp", "hp"))
})

test_that("columns = 'tidy' returns every cor.test() statistic, estimate renamed cor", {
  df <- mtcars[, c("mpg", "hp", "wt")]
  res <- corr_test_all(df, columns = "tidy", sort = "none")
  ref <- broom::tidy(stats::cor.test(df$mpg, df$hp))

  expect_named(res, c(
    "var1", "var2", "cor", "statistic", "p.value", "parameter",
    "conf.low", "conf.high", "method", "alternative"
  ))
  expect_equal(res$cor[1], ref$estimate)
  expect_equal(res$statistic[1], ref$statistic)
  expect_equal(res$conf.low[1], ref$conf.low)
  expect_equal(res$conf.high[1], ref$conf.high)
})

test_that("compact and tidy output report the same estimates", {
  df <- mtcars[, c("mpg", "hp", "wt", "qsec")]
  compact <- corr_test_all(df)
  tidy <- corr_test_all(df, columns = "tidy")

  expect_equal(compact$var1, tidy$var1)
  expect_equal(compact$var2, tidy$var2)
  expect_equal(compact$r, tidy$cor)
  expect_equal(compact$p_value, tidy$p.value)
})

test_that("sort orders rows as documented", {
  df <- mtcars[, c("mpg", "hp", "wt", "qsec")]
  unsorted <- corr_test_all(df, sort = "none")

  expect_false(is.unsorted(corr_test_all(df)$p_value))
  expect_equal(corr_test_all(df, sort = "estimate")$r, sort(unsorted$r, decreasing = TRUE))
  expect_equal(
    abs(corr_test_all(df, sort = "abs_estimate")$r),
    sort(abs(unsorted$r), decreasing = TRUE)
  )
})

test_that("use = 'complete.obs' tests every pair on the same rows", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    b = c(2, 1, 4, 3, 6, NA),
    c = c(NA, 1, 2, 3, 5, 4)
  )

  # cor.test()'s Pearson df is n - 2
  pairwise <- corr_test_all(df, columns = "tidy", sort = "none")
  expect_equal(unname(pairwise$parameter), c(3, 3, 2))

  complete <- corr_test_all(df, use = "complete.obs", columns = "tidy", sort = "none")
  expect_equal(unname(complete$parameter), c(2, 2, 2))

  expect_error(corr_test_all(df, use = "everything"), "should be one of")
})

test_that("a pair whose test cannot be computed is kept as NA", {
  df <- data.frame(a = c(1, 2, NA, NA), b = c(1, 2, 3, 5), c = c(4, 3, 2, 2))
  res <- corr_test_all(df, sort = "none")

  expect_equal(nrow(res), 3)
  expect_true(is.na(res$r[1]))
  expect_true(is.na(res$p_value[1]))
  expect_false(is.na(res$r[3]))
})

test_that("extra arguments reach cor.test()", {
  df <- mtcars[, c("mpg", "hp")]
  res <- corr_test_all(df, columns = "tidy", conf.level = 0.9)
  ref <- broom::tidy(stats::cor.test(df$mpg, df$hp, conf.level = 0.9))

  expect_equal(res$conf.low, ref$conf.low)
})
