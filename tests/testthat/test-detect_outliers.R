test_that("detect_outliers identifies IQR fence outliers", {
  x <- c(1, 2, 3, 4, 100)
  fences <- calculate_fences(x)
  expect_true(fences$upper_inner_fence < 100)

  # .outlier/.outlier_type are factors (matches the original implementation)
  flags <- flag_outliers(x)
  expect_equal(as.character(flags$.outlier[5]), "TRUE")
  expect_equal(as.character(flags$.outlier[1]), "FALSE")
  expect_equal(as.character(flags$.outlier_type[5]), "EXTREME")
  expect_equal(as.character(flags$.outlier_type[1]), "NONE")

  df <- data.frame(a = c(1, 2, 3, 4, 100))
  res <- detect_outliers(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$column, "a")
  expect_equal(res$value, 100)

  res_all <- detect_outliers(df, outliers_only = FALSE)
  expect_equal(nrow(res_all), 5)

  # Test numeric vector input directly
  res_vec <- detect_outliers(x)
  expect_equal(nrow(res_vec), 1)
  expect_equal(res_vec$value, 100)

  # Test empty and all-NA vectors
  fences_na <- calculate_fences(c(NA_real_, NA_real_))
  expect_true(is.na(fences_na$lower_inner_fence))

  flags_na <- flag_outliers(c(NA_real_, NA_real_))
  expect_equal(as.character(flags_na$.outlier), c("FALSE", "FALSE"))
  expect_equal(as.character(flags_na$.outlier_type), c("NONE", "NONE"))
})

test_that("a value on or inside the fence is not an outlier, so constant columns have none", {
  # every value sits on both fences when IQR = 0; `<=`/`>=` used to flag them all EXTREME
  const <- flag_outliers(c(5, 5, 5, 5))
  expect_false(any(as.logical(const$.outlier)))
  expect_true(all(const$.outlier_type == "NONE"))

  # the bulk of a zero-IQR column is ordinary; only the odd one out is flagged
  odd <- flag_outliers(c(0, 0, 0, 0, 1))
  expect_equal(as.character(odd$.outlier), c("FALSE", "FALSE", "FALSE", "FALSE", "TRUE"))
  expect_equal(as.character(odd$.outlier_type), c("NONE", "NONE", "NONE", "NONE", "EXTREME"))
  expect_equal(detect_outliers(data.frame(a = c(0, 0, 0, 0, 1)))$value, 1)

  # a point exactly on the inner fence is not flagged; just past it is MILD
  x <- c(0, 4, 5, 6, 7, 8, 9, 10, 20)
  f <- calculate_fences(x)
  on_fence <- x
  on_fence[9] <- f$upper_inner_fence
  expect_equal(as.character(flag_outliers(on_fence)$.outlier[9]), "FALSE")
  past <- x
  past[9] <- f$upper_inner_fence + 0.01
  expect_equal(as.character(flag_outliers(past)$.outlier_type[9]), "MILD")
})

test_that("detect_outliers on data with no numeric columns returns the documented empty schema", {
  res <- detect_outliers(data.frame(a = c("x", "y"), b = c(TRUE, FALSE)))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
  expect_named(res, c("column", "value", ".outlier", ".outlier_type"))

  res_all <- detect_outliers(data.frame(a = "x"), outliers_only = FALSE)
  expect_equal(nrow(res_all), 0L)
})
