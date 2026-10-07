test_that("cbe_cox_check works on a univariable coxph model", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)

  chk <- cbe_cox_check(fit)

  expect_s3_class(chk, "cbe_cox_check")
  expect_s3_class(chk$zph, "cox.zph")
  expect_s3_class(chk$zph_table, "data.frame")
  expect_type(chk$zph_violated, "logical")
  expect_named(chk$zph_violated, "age")
  expect_type(chk$zph_text, "character")
  expect_length(chk$zph_text, 1)

  expect_output(print(chk), "Proportional Hazards Assumption Check")
})

test_that("cbe_cox_check works on a multivariable coxph model, including the GLOBAL row", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)

  chk <- cbe_cox_check(fit)

  expect_equal(nrow(chk$zph_table), 3)
  expect_true("GLOBAL" %in% rownames(chk$zph_table))
  expect_named(chk$zph_violated, c("age", "sex"))
  expect_length(chk$zph_text, 2)
  expect_match(chk$zph_text[1], "Per-term proportional hazards tests")
  expect_match(chk$zph_text[2], "Global test")
})

test_that("cbe_cox_check dispatches on cbe_cox and cbe_cox_multi objects", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)

  res_single <- cbe_cox_single(lung, outcome = "outcome", feature = "age")
  chk_single <- cbe_cox_check(res_single)
  expect_s3_class(chk_single, "cbe_cox_check")

  res_multi <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex"))
  chk_multi <- cbe_cox_check(res_multi)
  expect_s3_class(chk_multi, "cbe_cox_check")
  expect_equal(nrow(chk_multi$zph_table), 3)
})

test_that("cbe_cox_check errors on unsupported input", {
  expect_error(cbe_cox_check(list(a = 1)), "must be a")
})
