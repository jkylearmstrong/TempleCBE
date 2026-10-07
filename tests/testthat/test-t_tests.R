test_that("single_t_test matches stats::t.test on an unpaired comparison", {
  df <- mtcars |> dplyr::mutate(am = factor(am))
  res <- single_t_test(df, "mpg", "am")

  ref <- stats::t.test(mpg ~ am, data = df)

  expect_equal(res$p.value, unname(ref$p.value))
  expect_equal(res$estimate1, unname(ref$estimate[1]))
  expect_equal(res$estimate2, unname(ref$estimate[2]))
  expect_equal(res$var, "mpg")
  expect_equal(res$fold_change, res$estimate2 / res$estimate1)
})

test_that("single_t_test errors when the grouping column doesn't have exactly 2 levels", {
  expect_error(single_t_test(iris, "Sepal.Length", "Species"), "exactly 2 levels")
})

test_that("single_t_test with paired = TRUE and .id pairs observations by subject, not row order", {
  # Group A rows are in subject order s1..s4; group B rows are DELIBERATELY
  # shuffled (s3, s1, s4, s2) to prove pairing follows `.id`, not position.
  df <- data.frame(
    subject = c("s1", "s2", "s3", "s4", "s3", "s1", "s4", "s2"),
    group = c("A", "A", "A", "A", "B", "B", "B", "B"),
    value = c(10, 20, 30, 40, 30.98, 11.00, 41.01, 21.02)
  )

  res <- single_t_test(df, "value", "group", .id = "subject", paired = TRUE)

  # Correctly paired by subject, the four differences are ~-1.0 with tiny
  # spread (-0.98, -1.0, -1.01, -1.02) -> a tight, highly significant result.
  expect_equal(unname(res$estimate), -1.0025, tolerance = 1e-6)
  expect_lt(res$p.value, 0.001)

  # Regression check: the mean of paired differences is invariant to pairing
  # order (mean(x) - mean(y) either way), so it alone can't distinguish
  # correct from naive pairing -- but the *spread* of those differences can.
  # Naive row-order pairing on this same (shuffled) data mixes unrelated
  # pairs, inflating the variance enormously and destroying significance,
  # even though the point estimate comes out identical.
  naive <- stats::t.test(
    df$value[df$group == "A"], df$value[df$group == "B"], paired = TRUE
  )
  expect_equal(unname(naive$estimate), unname(res$estimate), tolerance = 1e-6)
  expect_gt(naive$p.value, 0.5)
})

test_that("single_t_test with paired = TRUE doesn't crash on broom's paired-test output shape", {
  # Regression test: broom::tidy() on a *paired* t.test only returns a
  # single `estimate` column (the mean difference), not estimate1/estimate2
  # like the unpaired two-sample case. single_t_test() used to reference
  # .data$estimate1/.data$estimate2 unconditionally, so paired = TRUE
  # crashed on every call regardless of alignment.
  df <- data.frame(group = rep(c("A", "B"), each = 5), value = c(1, 2, 3, 4, 5, 3.1, 3.9, 5.2, 5.8, 7.3))
  res <- single_t_test(df, "value", "group", paired = TRUE)
  expect_true(is.numeric(res$fold_change))
  expect_false(is.na(res$fold_change))
})

test_that("single_t_test with paired = TRUE and .id errors on unmatched ids", {
  df <- data.frame(
    subject = c("s1", "s2", "s3"),
    group = c("A", "A", "B"), # s1 and s2 never appear in group B
    value = c(10, 20, 30)
  )
  expect_error(
    single_t_test(df, "value", "group", .id = "subject", paired = TRUE),
    "missing from one group"
  )
})

test_that("single_t_test with paired = TRUE and no .id warns about row-order pairing", {
  df <- data.frame(group = rep(c("A", "B"), each = 5), value = 1:10)
  expect_message(
    single_t_test(df, "value", "group", paired = TRUE),
    "row order"
  )
})

test_that("multiple_t_test runs single_t_test across every requested variable", {
  df <- mtcars |> dplyr::mutate(am = factor(am))
  res <- multiple_t_test(df, .var_list = c("mpg", "hp", "wt"), .class = "am")

  expect_equal(nrow(res), 3)
  expect_setequal(res$var, c("mpg", "hp", "wt"))
})

test_that("one_vs_rest_t_test runs one comparison per level of a multi-level factor", {
  res <- one_vs_rest_t_test(iris, "Sepal.Length", "Species")

  expect_equal(nrow(res), 3)
  expect_setequal(names(res), union(names(res), "var"))
  expect_true(all(grepl("mean in group (setosa|versicolor|virginica)", res$group1)))
})

test_that("single_t_test handles zero mean gracefully for fold_change", {
  df <- data.frame(
    val = c(0, 0, 0, 2, 4, 6),
    grp = factor(c("A", "A", "A", "B", "B", "B"))
  )
  res <- single_t_test(df, "val", "grp")
  expect_true(is.na(res$fold_change))
  expect_true(is.na(res$log2_fold_change))
})

test_that("multiple_t_test's default variable list leaves out the classifier and the id column", {
  # a numeric 0/1 classifier is the usual coding; it used to be 'tested' against itself
  res <- multiple_t_test(mtcars, .class = "am")
  expect_false("am" %in% res$var)
  expect_setequal(res$var, setdiff(names(mtcars), "am"))

  d <- data.frame(
    id = rep(1:6, 2), g = rep(c("pre", "post"), each = 6),
    sbp = c(120, 130, 125, 140, 135, 128, 118, 126, 121, 133, 130, 120),
    hr = c(70, 72, 68, 80, 75, 71, 66, 70, 65, 74, 72, 68)
  )
  expect_no_warning(res2 <- multiple_t_test(d, .class = "g", .id = "id", paired = TRUE))
  expect_setequal(res2$var, c("sbp", "hr"))
  expect_true(all(res2$method == "Paired t-test"))

  # an explicit list is used as given
  expect_equal(multiple_t_test(mtcars, .var_list = "mpg", .class = "am")$var, "mpg")
})

test_that("a paired t-test by .id drops an incomplete pair instead of aborting", {
  d <- data.frame(
    id = rep(1:5, 2), g = rep(c("pre", "post"), each = 5),
    v = c(1, 2, NA, 4, 5, 2, 3, 4, 5.5, 7)
  )
  res <- single_t_test(d, "v", "g", .id = "id", paired = TRUE)
  ref <- stats::t.test(c(1, 2, 4, 5), c(2, 3, 5.5, 7), paired = TRUE)   # id 3 has no complete pair
  expect_equal(unname(res$parameter), unname(ref$parameter))
  expect_equal(res$p.value, ref$p.value)
})

test_that("a paired t-test by .id rejects duplicated ids with a clear error", {
  d <- data.frame(id = c(1, 1, 2, 3, 1:3), g = rep(c("pre", "post"), c(4, 3)), v = 1:7)
  expect_no_warning(
    expect_error(single_t_test(d, "v", "g", .id = "id", paired = TRUE), "missing or duplicated ids")
  )
  # a three-level classifier pools several observations per id into '.rest'
  three <- data.frame(id = rep(1:4, 3), g = rep(c("a", "b", "c"), each = 4), v = c(1:4, 2:5, 3:6))
  expect_error(
    one_vs_rest_t_test(three, "v", "g", .id = "id", paired = TRUE),
    "missing or duplicated ids"
  )
})
