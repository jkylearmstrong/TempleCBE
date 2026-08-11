test_that("glmnet_IBS computes Integrated Brier Score correctly", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(2026)
  n <- 50
  df <- data.frame(
    time = runif(n, 10, 100),
    status = rbinom(n, 1, 0.7),
    age = rnorm(n, 50, 10),
    bmi = rnorm(n, 25, 4)
  )

  res <- glmnet_IBS(df)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("IBS", "lambda", "alpha") %in% names(res)))
  expect_true(is.numeric(res$IBS))
  expect_true(res$IBS >= 0 && res$IBS <= 1)

  # Edge case: NULL object
  res_null <- glmnet_IBS(NULL)
  expect_true(is.na(res_null$IBS))

  # rsplit object test
  if (requireNamespace("rsample", quietly = TRUE)) {
    spl <- rsample::initial_split(df)
    res_spl <- glmnet_IBS(spl)
    expect_s3_class(res_spl, "tbl_df")
    expect_true(is.numeric(res_spl$IBS))
  }
})
