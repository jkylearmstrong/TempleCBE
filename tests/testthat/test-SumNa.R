test_that("SumNa correctly counts NA values", {
  expect_equal(SumNa(c(1, 2, NA, 4, NA)), 2)
  expect_equal(SumNa(data.frame(a = c(1, NA), b = c(NA, NA))), 3)
  expect_equal(SumNa(c(1, 2, 3)), 0)
})
