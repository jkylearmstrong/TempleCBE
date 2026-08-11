test_that("z_norm standardizes data to mean 0 and sd 1", {
  x <- c(10, 20, 30, 40, 50)
  zx <- z_norm(x)
  expect_equal(mean(zx), 0)
  expect_equal(sd(zx), 1)
})

test_that("z_norm zeroes out constant (zero-variance) values but preserves NA positions", {
  # Regression test: the zero-variance branch used to return
  # rep(0, length(x)) unconditionally, silently turning original NAs into 0s.
  x <- c(5, 5, NA, 5, 5)
  zx <- z_norm(x)

  expect_equal(zx, c(0, 0, NA, 0, 0))
  expect_true(is.na(zx[3]))
})

test_that("z_norm handles a data frame, standardizing only numeric columns", {
  df <- data.frame(a = c(10, 20, 30), b = c(5, 5, 5), label = c("x", "y", "z"))
  res <- z_norm(df)

  expect_equal(mean(res$a), 0)
  expect_equal(res$b, c(0, 0, 0))
  expect_identical(res$label, df$label)
})
