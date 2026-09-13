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
