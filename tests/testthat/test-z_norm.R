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

test_that("z_norm with na.rm = FALSE returns NA for a column that has an NA, not 0", {
  # mean() is NA, so every z-score is undefined; these used to come back as 0
  expect_equal(z_norm(c(1, 2, NA, 4), na.rm = FALSE), rep(NA_real_, 4))
  df <- data.frame(a = c(1, 2, NA), b = c(1, 2, 3))
  res <- z_norm(df, na.rm = FALSE)
  expect_true(all(is.na(res$a)))
  expect_equal(mean(res$b), 0)
})

test_that("z_norm standardizes each matrix column on its own", {
  m <- matrix(c(1, 2, 3, 10, 20, 30), 3, dimnames = list(letters[1:3], c("u", "v")))
  z <- z_norm(m)
  expect_equal(dim(z), dim(m))
  expect_equal(dimnames(z), dimnames(m))
  expect_equal(unname(colMeans(z)), c(0, 0))
  expect_equal(unname(apply(z, 2, stats::sd)), c(1, 1))
  expect_equal(z[, "u"], z_norm(m[, "u"]))

  # integer input, a constant column, and NAs
  mi <- cbind(k = c(5L, 5L, 5L), n = c(1L, NA, 3L))
  zi <- z_norm(mi)
  expect_equal(unname(zi[, "k"]), c(0, 0, 0))
  expect_true(is.na(zi[2, "n"]))

  expect_error(z_norm(c("a", "b")), "numeric")
})
