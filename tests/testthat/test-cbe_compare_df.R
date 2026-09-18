test_that("cbe_compare_df identifies exact concordance", {
  df1 <- data.frame(id = 1:5, x = c(10, 20, 30, 40, 50), y = letters[1:5], stringsAsFactors = FALSE)
  df2 <- df1

  cmp <- cbe_compare_df(df1, df2, by = "id")
  expect_s3_class(cmp, "cbe_compare_df")
  expect_true(cmp$is_concordant)
  expect_equal(cmp$observations$n_matched, 5)
  expect_equal(cmp$observations$unmatched_base, 0)
  expect_equal(cmp$observations$unmatched_compare, 0)
  expect_equal(sum(cmp$summary$n_diff), 0)
  expect_equal(nrow(cmp$diffs), 0)
})

test_that("cbe_compare_df handles key-based alignment and unmatched keys", {
  df_base <- data.frame(id = c(1, 2, 3, 4), val = c(10, 20, 30, 40))
  # Reverse order and add extra key in compare, omit key 4
  df_comp <- data.frame(id = c(3, 1, 2, 5), val = c(30, 10, 20, 50))

  cmp <- cbe_compare_df(df_base, df_comp, by = "id")
  expect_false(cmp$is_concordant)
  expect_equal(cmp$observations$n_matched, 3)
  expect_equal(cmp$observations$unmatched_base, 1) # id 4
  expect_equal(cmp$observations$unmatched_compare, 1) # id 5
  # All matched values are identical
  expect_equal(sum(cmp$summary$n_diff), 0)
})

test_that("cbe_compare_df enforces numeric tolerance", {
  df1 <- data.frame(id = 1:3, x = c(1.000, 2.000, 3.000))
  df2 <- data.frame(id = 1:3, x = c(1.0001, 2.000, 3.050))

  # Loose tolerance (0.01): row 1 matches, row 3 fails
  cmp_loose <- cbe_compare_df(df1, df2, by = "id", tolerance = 0.01)
  expect_equal(cmp_loose$summary$n_diff[cmp_loose$summary$variable == "x"], 1)
  expect_equal(nrow(cmp_loose$diffs), 1)
  expect_equal(cmp_loose$diffs$id, 3)

  # Tight tolerance (1e-5): row 1 and 3 fail
  cmp_tight <- cbe_compare_df(df1, df2, by = "id", tolerance = 1e-5)
  expect_equal(cmp_tight$summary$n_diff[cmp_tight$summary$variable == "x"], 2)
  expect_equal(nrow(cmp_tight$diffs), 2)
})

test_that("cbe_compare_df handles missing values (NA) appropriately", {
  df1 <- data.frame(id = 1:3, num = c(1, NA, 3), char = c("a", NA, "c"), stringsAsFactors = FALSE)
  df2 <- data.frame(id = 1:3, num = c(1, NA, 4), char = c("a", "b", "c"), stringsAsFactors = FALSE)

  cmp <- cbe_compare_df(df1, df2, by = "id")
  # num: id 2 (both NA) matches; id 3 differs
  # char: id 2 (one NA, one 'b') differs
  expect_equal(cmp$summary$n_diff[cmp$summary$variable == "num"], 1)
  expect_equal(cmp$summary$n_diff[cmp$summary$variable == "char"], 1)
})

test_that("cbe_compare_df captures variable labels from labelled", {
  df1 <- data.frame(id = 1:2, bmi = c(22.1, 27.4))
  df2 <- df1
  attr(df1$bmi, "label") <- "Body Mass Index (kg/m2)"

  cmp <- cbe_compare_df(df1, df2, by = "id")
  expect_equal(cmp$summary$label[cmp$summary$variable == "bmi"], "Body Mass Index (kg/m2)")
})

test_that("cbe_compare_df S3 methods work as expected", {
  df1 <- data.frame(id = 1:3, val = c(10, 20, 30))
  df2 <- data.frame(id = 1:3, val = c(10, 25, 30))

  cmp <- cbe_compare_df(df1, df2, by = "id")

  # Print method
  expect_output(print(cmp), "PROC COMPARE Parity")
  expect_output(print(cmp), "Variables with Differences: 1 / 1")

  # Summary method
  s <- summary(cmp)
  expect_s3_class(s, "tbl_df")
  expect_equal(s$n_diff, 1)

  # Tidy method
  td <- generics::tidy(cmp)
  expect_s3_class(td, "tbl_df")
  expect_equal(nrow(td), 1)
  expect_equal(td$id, 2)
  expect_equal(td$diff, -5)
})

test_that("cbe_compare_df handles tables with no non-key columns without warning", {
  # A bare edge list: `by` covers every column, so there are zero non-key
  # variables to compare -- summary_list stays empty, which used to make
  # bind_rows() return a columnless tibble and warn on every $n_diff access.
  edges_x <- data.frame(from = c("a", "b"), to = c("b", "c"))
  edges_y <- data.frame(from = "a", to = "b")

  cmp <- expect_no_warning(
    cbe_compare_df(edges_x, edges_y, by = c("from", "to"))
  )
  expect_equal(nrow(cmp$summary), 0)
  expect_true(all(c("variable", "n_diff", "types_match") %in% names(cmp$summary)))
  expect_equal(cmp$observations$n_matched, 1)
  expect_equal(cmp$observations$unmatched_base, 1)
  expect_false(cmp$is_concordant) # unmatched_base > 0
  expect_output(print(cmp), "All values match within tolerance")
})

test_that("cbe_compare_df errors on invalid by keys", {
  df1 <- data.frame(id = 1:2, x = 1:2)
  df2 <- data.frame(idx = 1:2, x = 1:2)

  expect_error(cbe_compare_df(df1, df2, by = "id"), "Key variable.*not found in compare dataset")
  expect_error(cbe_compare_df(df1, df2, by = "nonexistent"), "Key variable.*not found in base dataset")
})
