test_that("cbe_cox_table formats a cbe_cox object and adds a log(HR) column", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  tbl <- cbe_cox_table(res)

  expect_s3_class(tbl, "tbl_df")
  expect_true("log(HR)" %in% names(tbl))
  expect_equal(nrow(tbl), nrow(res$table))
})

test_that("cbe_cox_table formats a cbe_cox_multi object", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))
  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex_f"))

  tbl <- cbe_cox_table(res)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), nrow(res$table))
})

test_that("cbe_cox_table sort = 'pvalue' orders ascending by p-value within variable", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "ph.ecog"))

  tbl <- cbe_cox_table(res, sort = "pvalue")
  p_num <- suppressWarnings(as.numeric(gsub("^<", "", tbl$p.value)))
  expect_true(all(diff(p_num[!is.na(p_num)]) >= 0))
})

test_that("cbe_cox_table sort = 'magnitude' orders by descending abs(log(HR))", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "ph.ecog")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "ph.ecog"))

  tbl <- cbe_cox_table(res, sort = "magnitude")
  mags <- abs(tbl[["log(HR)"]])
  expect_true(all(diff(mags) <= 0))
})

test_that("cbe_cox_table significance = TRUE adds a sig column and bolds significant p-values", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "sex")

  tbl <- cbe_cox_table(res, significance = TRUE)
  expect_true("sig" %in% names(tbl))
  expect_true(any(nzchar(tbl$sig)))
})

test_that("cbe_cox_table errors on unsupported input", {
  expect_error(cbe_cox_table(list(a = 1)), "must be a")
})
