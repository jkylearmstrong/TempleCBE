fits <- function() {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  list(
    age = survival::coxph(survival::Surv(time, status) ~ age, data = lung),
    both = survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung),
    full = survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  )
}

test_that("tbl_cox_glance_compare() reports the glance metrics, sorted by AIC", {
  m <- fits()
  tab <- tbl_cox_glance_compare(m)
  expect_s3_class(tab, "cbe_cox_glance")
  expect_named(tab, c("Model", "N", "Events", "Log-Likelihood", "AIC", "BIC", "Concordance (SE)"))
  num <- attr(tab, "numeric")
  expect_false(is.unsorted(num$AIC))
  g <- broom::glance(m$both)
  row <- num[num$model == "both", ]
  expect_equal(row$AIC, g$AIC)
  expect_equal(row$BIC, g$BIC)
  expect_equal(row$loglik, as.numeric(g$logLik))
  expect_equal(row$n, g$n)
  expect_equal(row$events, g$nevent)
  txt <- tab[tab$Model == "both", ]
  expect_equal(txt$AIC, sprintf("%.2f", g$AIC))
  expect_equal(txt$`Concordance (SE)`, sprintf("%.2f (%.2f)", g$concordance, g$std.error.concordance))
})

test_that("sort_by, names and digits work", {
  m <- fits()
  by_c <- attr(tbl_cox_glance_compare(m, sort_by = "concordance"), "numeric")
  expect_false(is.unsorted(rev(by_c$concordance)))
  by_b <- attr(tbl_cox_glance_compare(m, sort_by = "BIC"), "numeric")
  expect_false(is.unsorted(by_b$BIC))
  keep <- tbl_cox_glance_compare(unname(m), sort_by = NULL)
  expect_equal(keep$Model, c("Model 1", "Model 2", "Model 3"))
  named <- tbl_cox_glance_compare(unname(m), names = c("a", "b", "c"), sort_by = NULL)
  expect_equal(named$Model, c("a", "b", "c"))
  expect_match(tbl_cox_glance_compare(m, digits = 0)$AIC[1], "^[0-9]+$")
  expect_error(tbl_cox_glance_compare(unname(m), names = "x"), "one name per model")
  expect_error(tbl_cox_glance_compare(m, sort_by = "R2"))
})

test_that("a single model, cox_single() results and bad input are handled", {
  m <- fits()
  expect_equal(nrow(tbl_cox_glance_compare(m$age)), 1L)
  lung <- survival::lung
  lung$outcome <- survival::Surv(lung$time, lung$status)
  cs <- cox_single(lung, outcome = "outcome", feature = "age")
  expect_equal(nrow(suppressWarnings(tbl_cox_glance_compare(list(cs, m$age)))), 2L)
  expect_error(tbl_cox_glance_compare(list(1)), "coxph fit")
  expect_error(tbl_cox_glance_compare(list()), "non-empty")
  expect_error(tbl_cox_glance_compare(m, digits = -1), "non-negative")
})

test_that("models fitted to different rows trigger a comparability warning", {
  lung <- survival::lung
  a <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
  b <- survival::coxph(survival::Surv(time, status) ~ age + ph.ecog, data = lung)
  expect_warning(tbl_cox_glance_compare(list(a, b)), "not comparable")
})
