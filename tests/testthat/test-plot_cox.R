test_that("plot_cox_forest generates a valid ggplot object", {
  test_df <- data.frame(
    index_label = c("Age", "Biomarker A", "Biomarker B"),
    estimate    = c(1.2, 0.8, 1.5),
    conf.low    = c(1.05, 0.65, 1.1),
    conf.high   = c(1.38, 0.98, 2.05),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, title = "Hazard Ratios")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_survival works with cbe_cox object", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_survival(res, data = lung)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_marginal works with cbe_cox object and continuous predictor", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, status_col = "status")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_survival and plot_cox_marginal error when feature is missing with raw coxph", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)

  expect_error(plot_cox_survival(fit, data = lung), "Must specify 'feature'")
  expect_error(plot_cox_marginal(fit, data = lung), "Must specify 'feature'")
})
