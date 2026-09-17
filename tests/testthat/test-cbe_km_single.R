test_that("cbe_km_single works on a numeric predictor, binning it into quartiles for KM strata", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res <- cbe_km_single(lung, outcome = "outcome", feature = "age")

  expect_s3_class(res, "cbe_km")
  expect_s3_class(res$cox, "cbe_cox")
  expect_s3_class(res$km_fit, "survfit")
  expect_s3_class(res$km_tidy, "data.frame")
  expect_equal(length(res$km_fit$strata), 4)
  expect_true(res$direction %in% c("increases", "decreases"))
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), 4)
  expect_true("km_median_time" %in% names(res$summary))

  expect_output(print(res), "Kaplan-Meier & Cox PH Summary")
})

test_that("cbe_km_single works on a factor predictor, using its levels directly as KM strata", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

  res <- cbe_km_single(lung, outcome = "outcome", feature = "sex_f")

  expect_false(res$cox$is_numeric)
  expect_equal(length(res$km_fit$strata), 2)
  expect_equal(nrow(res$summary), 2)
  expect_setequal(res$summary$Level, c("Male", "Female"))
  expect_true(all(c("HR", "95% CI", "p.value") %in% names(res$summary)))
})

test_that("cbe_km_single does not refit the Cox model separately from cbe_cox_single", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res_km <- cbe_km_single(lung, outcome = "outcome", feature = "age")
  res_cox <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  expect_equal(stats::coef(res_km$cox$model), stats::coef(res_cox$model))
})
