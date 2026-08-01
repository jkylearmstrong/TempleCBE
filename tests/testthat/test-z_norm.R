test_that("z_norm standardizes data to mean 0 and sd 1", {
  x <- c(10, 20, 30, 40, 50)
  zx <- z_norm(x)
  expect_equal(mean(zx), 0)
  expect_equal(sd(zx), 1)
})
