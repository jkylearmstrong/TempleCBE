test_that("cbe_cox_single works on numeric continuous predictors", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  expect_s3_class(res, "cbe_cox")
  expect_true(res$is_numeric)
  expect_equal(res$feature, "age")
  expect_equal(res$var_label, "age")
  expect_s3_class(res$model, "coxph")
  expect_type(res$zph_text, "character")
  expect_type(res$zph_violated, "logical")
  expect_s3_class(res$table, "data.frame")
  expect_equal(nrow(res$table), 1)
  expect_equal(res$table$Role, "Covariate")
  expect_type(res$interpretation, "character")
  expect_s3_class(res$glance, "data.frame")

  # Print method
  expect_output(print(res), "Cox PH Model for: age")
})

test_that("cbe_cox_single works on categorical factor predictors with explicit reference rows", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

  res <- cbe_cox_single(lung, outcome = "outcome", feature = "sex_f")

  expect_s3_class(res, "cbe_cox")
  expect_false(res$is_numeric)
  expect_equal(nrow(res$table), 2)
  expect_equal(res$table$Role, c("Reference", "Comparison"))
  expect_equal(res$table$Level, c("Male", "Female"))
  expect_equal(res$table$HR[1], 1.0)
  expect_equal(res$table$`95% CI`[1], "Reference")
  expect_match(res$interpretation, "Female.*vs\\. reference.*Male")
})

test_that("cbe_cox_single uses labelled variable labels when present", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  labelled::var_label(lung$age) <- "Patient Age at Diagnosis (Years)"

  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")
  expect_equal(res$var_label, "Patient Age at Diagnosis (Years)")
  expect_match(res$interpretation, "Patient Age at Diagnosis \\(Years\\)")
  expect_equal(res$table$Variable[1], "Patient Age at Diagnosis (Years)")
})

test_that("cbe_cox_single errors on missing feature", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  expect_error(
    cbe_cox_single(lung, outcome = "outcome", feature = "nonexistent_var"),
    "not found in provided data"
  )
})

test_that("cbe_cox_single labels a non-default interval for a numeric predictor, not '95% CI'", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res90 <- cbe_cox_single(lung, outcome = "outcome", feature = "age", conf_level = 0.90)
  expect_true("90% CI" %in% names(res90$table))
  expect_false("95% CI" %in% names(res90$table))

  # ... and the interval in that column really is the 90% one
  ref <- summary(survival::coxph(outcome ~ age, lung), conf.int = 0.90)$conf.int
  expect_equal(res90$table[["90% CI"]], sprintf("%.2f – %.2f", ref[1, 3], ref[1, 4]))
  expect_match(res90$interpretation, "90% CI", fixed = TRUE)

  # the default is unchanged, and matches the factor branch's convention
  expect_true("95% CI" %in% names(cbe_cox_single(lung, outcome = "outcome", feature = "age")$table))
  expect_true("90% CI" %in% names(cbe_cox_single(lung, outcome = "outcome", feature = "sex", conf_level = 0.90)$table))
})
