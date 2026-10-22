test_that("SumNa correctly counts NA values", {
  expect_equal(count_na(c(1, 2, NA, 4, NA)), 2)
  expect_equal(count_na(data.frame(a = c(1, NA), b = c(NA, NA))), 3)
  expect_equal(count_na(c(1, 2, 3)), 0)
})

test_that("SumNa correctly counts sentinel values in vectors and data frames", {
  df <- data.frame(a = c(1, 2, NA), b = c("999", "-99", "valid"))
  expect_equal(count_na(df, na_list = c("999", "-99")), 3)
  expect_equal(count_na(df$b, na_list = c("999", "-99")), 2)
  expect_equal(count_na(df$a, na_list = c("999")), 1)
})
