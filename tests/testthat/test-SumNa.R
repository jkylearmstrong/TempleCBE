test_that("SumNa correctly counts NA values", {
  expect_equal(SumNa(c(1, 2, NA, 4, NA)), 2)
  expect_equal(SumNa(data.frame(a = c(1, NA), b = c(NA, NA))), 3)
  expect_equal(SumNa(c(1, 2, 3)), 0)
})

test_that("SumNa correctly counts sentinel values in vectors and data frames", {
  df <- data.frame(a = c(1, 2, NA), b = c("999", "-99", "valid"))
  expect_equal(SumNa(df, na_list = c("999", "-99")), 3)
  expect_equal(SumNa(df$b, na_list = c("999", "-99")), 2)
  expect_equal(SumNa(df$a, na_list = c("999")), 1)
})
