test_that("missmap returns a ggplot object for the per-row/column view", {
  df <- data.frame(
    a = c(1, NA, 3, 4, NA),
    b = c(NA, NA, 3, 4, 5)
  )
  p <- missmap(df)
  expect_s3_class(p, "ggplot")
})

test_that("missmap returns a ggplot object for the by_column aggregated view", {
  df <- data.frame(
    a = c(1, NA, 3, 4, NA),
    b = c(NA, NA, 3, 4, 5),
    site = c("A", "A", "B", "B", "B")
  )
  p <- missmap(df, by_column = site)
  expect_s3_class(p, "ggplot")
})

test_that("missmap treats na_list values as missing in addition to real NA", {
  df <- data.frame(a = c("1", "NA", "3"), b = c("", "2", "3"))
  p <- missmap(df, na_list = c("NA", ""))

  missing_count <- sum(p$data$is_na)
  expect_equal(missing_count, 2)
})

test_that("missmap errors on input that can't be coerced to a tibble", {
  expect_error(missmap(environment()), "cannot be coerced")
})
