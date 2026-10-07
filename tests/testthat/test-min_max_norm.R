test_that("min_max_norm correctly scales numeric data", {
  x <- c(10, 20, 30, 40, 50)
  norm_x <- min_max_norm(x)
  expect_equal(min(norm_x), 0)
  expect_equal(max(norm_x), 1)
  expect_equal(norm_x[3], 0.5)
  
  # non-numeric columns are dropped by design (matches the original
  # Internal_Protected_DataStore implementation this was migrated from) -- range_norm() is the
  # variant that keeps them
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
  norm_df <- min_max_norm(df)
  expect_equal(norm_df$a, c(0, 0.5, 1))
  expect_null(norm_df$b)

  norm_df2 <- range_norm(df)
  expect_equal(norm_df2$b, c("x", "y", "z"))
})

test_that("min_max_norm preserves NA values while scaling valid observations", {
  x <- c(10, 20, NA, 40, 50)
  norm_x <- min_max_norm(x)
  expect_equal(min(norm_x, na.rm = TRUE), 0)
  expect_equal(max(norm_x, na.rm = TRUE), 1)
  expect_true(is.na(norm_x[3]))
  expect_equal(norm_x[2], 0.25)
})

test_that("min_max_norm handles constant, all-missing, and non-finite input", {
  # zero range: every finite value maps to 0, not NaN
  expect_equal(min_max_norm(c(5, 5, 5)), c(0, 0, 0))
  expect_equal(min_max_norm(c(5, NA, 5)), c(0, NA, 0))

  # nothing finite to scale against: all NA, and no warning
  expect_no_warning(all_na <- min_max_norm(c(NA_real_, NA_real_)))
  expect_equal(all_na, c(NA_real_, NA_real_))

  # Inf/-Inf neither define the range nor get rescaled
  expect_equal(min_max_norm(c(0, 5, 10, Inf)), c(0, 0.5, 1, Inf))

  # names survive; logical input is treated as 0/1
  expect_equal(names(min_max_norm(c(a = 1, b = 3))), c("a", "b"))
  expect_equal(min_max_norm(c(TRUE, FALSE, TRUE)), c(1, 0, 1))

  # non-numeric vectors get a clear error, not "non-numeric argument to binary operator"
  expect_error(min_max_norm(c("a", "b")), "must be numeric")

  # per-column: a constant column does not turn the others into NaN
  df <- data.frame(a = c(1, 2, 3), k = c(7, 7, 7))
  out <- min_max_norm(df)
  expect_equal(out$a, c(0, 0.5, 1))
  expect_equal(out$k, c(0, 0, 0))
})

test_that("range_norm handles a constant or all-missing frame without NaN or warnings", {
  expect_equal(range_norm(data.frame(x = c(2, 2), y = c(2, 2))), data.frame(x = c(0, 0), y = c(0, 0)))
  expect_no_warning(out <- range_norm(data.frame(x = c(NA_real_, NA_real_))))
  expect_true(all(is.na(out$x)))

  # one global range: x and y are scaled together
  out2 <- range_norm(data.frame(x = c(0, 5), y = c(5, 10)))
  expect_equal(out2$x, c(0, 0.5))
  expect_equal(out2$y, c(0.5, 1))
})
