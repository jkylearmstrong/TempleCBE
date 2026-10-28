# tbl_schoenfeld_audit(), pool_imputations(), tbl_cumulative_incidence()

test_that("tbl_schoenfeld_audit returns term rows plus GLOBAL and flags by alpha", {
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, data = survival::lung)
  a <- tbl_schoenfeld_audit(fit)
  expect_equal(a$term, c("age", "sex", "ph.ecog", "GLOBAL"))
  expect_type(a$non_proportional, "logical")
  expect_equal(a$non_proportional, a$p.value < 0.05)
  expect_true(all(!tbl_schoenfeld_audit(fit, alpha = 1e-12)$non_proportional))
  expect_error(tbl_schoenfeld_audit(lm(mpg ~ wt, mtcars)), "coxph")
})

test_that("pool_imputations applies Rubin's rules", {
  fits <- list(
    data.frame(term = "x", estimate = 1, std.error = 0.5),
    data.frame(term = "x", estimate = 2, std.error = 0.5),
    data.frame(term = "x", estimate = 3, std.error = 0.5)
  )
  p <- pool_imputations(fits)
  expect_equal(p$estimate, 2)
  expect_equal(p$within_var, 0.25)
  expect_equal(p$between_var, 1)
  expect_equal(p$total_var, 0.25 + (1 + 1 / 3) * 1)
  r <- (1 + 1 / 3) * 1 / 0.25
  expect_equal(p$df, 2 * (1 + 1 / r)^2)
  expect_equal(p$conf.low, p$estimate - qt(0.975, p$df) * p$std.error)

  stacked <- do.call(rbind, Map(function(d, i) cbind(imputation = i, d), fits, 1:3))
  expect_equal(pool_imputations(stacked)$estimate, 2)
  expect_equal(pool_imputations(fits, exponentiate = TRUE)$estimate, exp(2))
  expect_error(pool_imputations(fits[1]), "two imputations")
  expect_error(pool_imputations(list(fits[[1]], data.frame(term = "x"))), "std.error")
})

test_that("tbl_cumulative_incidence: causes sum to 1 - overall survival and groups split", {
  set.seed(2)
  d <- data.frame(time = rexp(300, 0.1), type = sample(0:2, 300, TRUE), arm = rep(c("A", "B"), 150))
  r <- tbl_cumulative_incidence(d, "time", "type", times = c(5, 10))
  expect_equal(sort(unique(r$cause)), c("1", "2"))
  tot <- tapply(r$cif, r$time, sum)
  km <- summary(survival::survfit(survival::Surv(time, type > 0) ~ 1, d), times = c(5, 10), extend = TRUE)$surv |> as.vector()
  expect_equal(as.vector(tot), 1 - km, tolerance = 1e-8)
  g <- tbl_cumulative_incidence(d, "time", "type", group_col = "arm", times = 5)
  expect_equal(nrow(g), 4L)
  expect_true(all(g$conf.low <= g$cif + 1e-12 & g$cif <= g$conf.high + 1e-12))
  expect_error(tbl_cumulative_incidence(d[d$type < 2, ], "time", "type"), "at least two causes")
})
