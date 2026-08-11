test_that("ipcw_brier_score matches a hand-computed value", {
  skip_if_not_installed("survival")

  # No censoring in training -> censoring survival G(t) = 1 for all t,
  # so IPCW weights all collapse to 1 and the arithmetic is easy to check by hand.
  cens_fit <- survival::survfit(survival::Surv(c(1, 2, 3, 4, 5), rep(0, 5)) ~ 1)
  expect_equal(censoring_survival_prob(3, cens_fit), 1)

  # subject 1: event at t=2 (<=3)   -> (0 - 0.5)^2 = 0.25
  # subject 2: event at t=10 (>3)   -> at risk      -> (1 - 0.5)^2 = 0.25
  # subject 3: event at t=10 (>3)   -> at risk      -> (1 - 0.9)^2 = 0.01
  # BS(3) = (0.25 + 0.25 + 0.01) / 3 = 0.17
  bs <- ipcw_brier_score(3, obs_time = c(2, 10, 10), status = c(1, 1, 1),
                          p_hat = c(0.5, 0.5, 0.9), cens_fit = cens_fit)
  expect_equal(bs, 0.17, tolerance = 1e-8)
})

test_that("ipcw_brier_score normalizes by the full sample size, not just contributing subjects", {
  # Regression test for a prior bug where the denominator was the sum of
  # IPCW weights among subjects that contributed at t, instead of n. A
  # subject censored before t contributes 0 to the numerator but must
  # still count in the denominator like every other subject.
  skip_if_not_installed("survival")
  cens_fit <- survival::survfit(survival::Surv(c(1, 2, 3, 4, 5), rep(0, 5)) ~ 1)

  # subject 1: censored at t=1 (<=5, status=0) -> excluded, contributes 0
  # subject 2: at risk at t=5 (obs_time=10>5)  -> (1 - 0.5)^2 = 0.25
  # correct BS(5) = 0.25 / 2 = 0.125; the old (buggy) w_sum-normalized
  # version would have returned 0.25 / 1 = 0.25 (double).
  bs <- ipcw_brier_score(5, obs_time = c(1, 10), status = c(0, 1),
                          p_hat = c(0.5, 0.5), cens_fit = cens_fit)
  expect_equal(bs, 0.125, tolerance = 1e-8)
})

test_that("integrate_brier_score performs trapezoidal integration normalized by time range", {
  expect_equal(integrate_brier_score(c(1, 2), c(0.2, 0.4)), 0.3, tolerance = 1e-8)
  expect_equal(integrate_brier_score(5, 0.3), 0.3) # single time point falls back to mean
})

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
