test_that("sd.error computes sample standard error correctly", {
  v <- c(10, 20, 30, 40, 50)
  se <- sd.error(v)
  expected_se <- sd(v) / sqrt(length(v))
  expect_equal(se, expected_se)

  # Test na.rm handling
  v_na <- c(10, 20, 30, NA, 50)
  se_na <- sd.error(v_na, na.rm = TRUE)
  expected_se_na <- sd(v_na, na.rm = TRUE) / sqrt(4)
  expect_equal(se_na, expected_se_na)
})
