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

test_that("missmap by_column orders groups and features by descending missingness when row_order = FALSE", {
  df <- data.frame(
    a = c(1, NA, 3, NA),
    b = c(NA, NA, 3, 4),
    c = c(1, 2, 3, 4),
    site = c("A", "A", "B", "B")
  )
  p <- missmap(df, by_column = site, row_order = FALSE)

  # feature 'b' has the most missingness overall (2), then 'a' (2 as well),
  # then 'c' (0) -- check that features are a factor ordered by descending
  # total missingness (most-missing features first).
  feature_levels <- levels(p$data$variable)
  feature_sums <- vapply(feature_levels, function(f) {
    sum(p$data$sum_na[p$data$variable == f])
  }, numeric(1))
  expect_equal(feature_sums, sort(feature_sums, decreasing = TRUE), ignore_attr = TRUE)

  # group 'A' has more missingness (a: 1, b: 2 -> 3) than group 'B' (a: 1, b: 0 -> 1)
  group_levels <- levels(p$data$site)
  expect_equal(group_levels[1], "A")
})

test_that("missmap by_column with row_order = TRUE leaves groups/features unordered (not factors)", {
  df <- data.frame(
    a = c(1, NA, 3, NA),
    b = c(NA, NA, 3, 4),
    site = c("A", "A", "B", "B")
  )
  p <- missmap(df, by_column = site, row_order = TRUE)
  expect_false(is.factor(p$data$site))
  expect_false(is.factor(p$data$variable))
})

test_that("missmap by_column uses a discrete Missing/Present fill when every group has <= 1 row", {
  df <- data.frame(
    a = c(1, NA, 3, 4),
    b = c(NA, NA, 3, 4),
    subject_id = c("s1", "s2", "s3", "s4")
  )
  p <- missmap(df, by_column = subject_id)

  expect_true(is.factor(p$data$Missing))
  expect_equal(levels(p$data$Missing), c("Missing", "Present"))
  expect_identical(p$labels$fill, "Data Status")
})

test_that("missmap by_column keeps the continuous gradient fill when a group has > 1 row (regression)", {
  df <- data.frame(
    a = c(1, NA, 3, 4, NA),
    b = c(NA, NA, 3, 4, 5),
    site = c("A", "A", "B", "B", "B")
  )
  p <- missmap(df, by_column = site)

  expect_true(is.numeric(p$data$sum_na))
  expect_false("Missing" %in% names(p$data))
  expect_identical(p$labels$fill, "# missing")
})

test_that("missmap by_column fill argument can force binary/count behavior explicitly", {
  df_multi <- data.frame(
    a = c(1, NA, 3, 4, NA),
    b = c(NA, NA, 3, 4, 5),
    site = c("A", "A", "B", "B", "B")
  )
  p_forced_binary <- missmap(df_multi, by_column = site, fill = "binary")
  expect_true(is.factor(p_forced_binary$data$Missing))
  expect_identical(p_forced_binary$labels$fill, "Data Status")

  df_single <- data.frame(
    a = c(1, NA, 3, 4),
    b = c(NA, NA, 3, 4),
    subject_id = c("s1", "s2", "s3", "s4")
  )
  p_forced_count <- missmap(df_single, by_column = subject_id, fill = "count")
  expect_true(is.numeric(p_forced_count$data$sum_na))
  expect_identical(p_forced_count$labels$fill, "# missing")
})
