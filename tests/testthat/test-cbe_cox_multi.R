test_that("cbe_cox_multi works with numeric and factor predictors via outcome/features", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex_f"))

  expect_s3_class(res, "cbe_cox_multi")
  expect_s3_class(res$model, "coxph")
  expect_equal(res$features, c("age", "sex_f"))
  expect_s3_class(res$table, "data.frame")
  expect_equal(nrow(res$table), 3) # age (1 row) + sex_f (reference + comparison)
  expect_equal(res$table$Role, c("Covariate", "Reference", "Comparison"))
  expect_equal(res$table$Level[res$table$Variable == "sex_f"], c("Male", "Female"))
  expect_equal(res$table$HR[res$table$Role == "Reference"], 1.00)
  expect_true("log(HR)" %in% names(res$table))
  expect_s3_class(res$glance, "data.frame")
  expect_s3_class(res$zph, "cbe_cox_check")
  expect_true(is.logical(res$converged))
  expect_true(is.numeric(res$n_iterations))

  expect_output(print(res), "Multivariable Cox PH Model")
})

test_that("cbe_cox_multi accepts a formula interface equivalent to outcome/features", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res_formula <- cbe_cox_multi(lung, formula = survival::Surv(time, status) ~ age + sex)
  res_features <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex"))

  expect_equal(res_formula$features, c("age", "sex"))
  expect_equal(
    unname(stats::coef(res_formula$model)),
    unname(stats::coef(res_features$model))
  )
})

test_that("cbe_cox_multi uses labelled variable labels when present", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  labelled::var_label(lung$age) <- "Patient Age at Diagnosis (Years)"

  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex"))
  expect_equal(res$var_labels[["age"]], "Patient Age at Diagnosis (Years)")
  expect_true("Patient Age at Diagnosis (Years)" %in% res$table$Variable)
})

test_that("cbe_cox_multi errors clearly without formula or features, and on missing features", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  expect_error(cbe_cox_multi(lung), "Must supply either")
  expect_error(
    cbe_cox_multi(lung, outcome = "outcome", features = c("age", "nonexistent_var")),
    "not found in provided data"
  )
})

test_that("cbe_cox_multi reports non-convergence when a custom iteration limit is exhausted", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  # too few iterations: coxph() warns and the fit has not converged. This used to
  # be compared against the *default* limit of 20, so it read "converged".
  expect_warning(
    res_short <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex", "ph.ecog"),
                               control = survival::coxph.control(iter.max = 2)),
    "Ran out of iterations"
  )
  expect_false(res_short$converged)
  expect_output(print(res_short), "Converged: No")

  # a generous limit that the fit does not exhaust is still reported as converged
  res_ok <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex", "ph.ecog"),
                          control = survival::coxph.control(iter.max = 50))
  expect_true(res_ok$converged)
  expect_true(cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex"))$converged)
})

test_that("cbe_cox_multi warns about coefficients its table cannot show", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.karno")])

  expect_warning(
    res <- cbe_cox_multi(lung, formula = survival::Surv(time, status) ~ age * factor(sex) + log(ph.karno)),
    "not in `table`.*log\\(ph.karno\\)"
  )
  expect_true(all(c("age:factor(sex)2", "log(ph.karno)") %in% names(stats::coef(res$model))))

  # plain main effects (numeric and factor) raise no warning
  lung$sex_f <- factor(lung$sex, labels = c("Male", "Female"))
  expect_no_warning(
    cbe_cox_multi(lung, formula = survival::Surv(time, status) ~ age + sex_f + ph.karno)
  )
})
