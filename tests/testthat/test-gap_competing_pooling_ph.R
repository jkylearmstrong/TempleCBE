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

test_that("input validation errors are informative", {
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = survival::lung)
  expect_error(tbl_schoenfeld_audit(fit, alpha = 2), "alpha")
  d <- data.frame(time = c(1, 2, 3, 4), type = c(0, 1, 2, 1), arm = "A")
  expect_error(tbl_cumulative_incidence("x", "time", "type"), "data frame")
  expect_error(tbl_cumulative_incidence(d, "nope", "type"), "not a column")
  expect_error(tbl_cumulative_incidence(d, "time", "type", times = -1), "non-negative")
  expect_error(tbl_cumulative_incidence(d, "time", "type", conf_level = 2), "conf_level")
  est <- data.frame(term = "x", estimate = 1, std.error = 1)
  expect_error(pool_imputations(est), "imputation")
  expect_error(pool_imputations(list(est, est), conf_level = 0), "conf_level")
  expect_error(pool_imputations(list(est, data.frame(term = "y", estimate = 1, std.error = 1))), "missing")
  expect_equal(pool_imputations(list(est, est))$df, Inf)
})

test_that("pool_imputations pools terms absent from the first imputation and applies Barnard-Rubin", {
  fits <- list(
    data.frame(term = "x", estimate = 1, std.error = 0.5),
    data.frame(term = c("x", "z"), estimate = c(2, 1), std.error = 0.5),
    data.frame(term = c("x", "z"), estimate = c(3, 2), std.error = 0.5)
  )
  expect_error(pool_imputations(fits), "`z`")

  full <- lapply(1:3, function(i) data.frame(term = "x", estimate = i, std.error = 0.5))
  big <- pool_imputations(full)
  small <- pool_imputations(full, df_complete = 10)
  tot <- big$total_var
  lambda <- (1 + 1 / 3) * 1 / tot
  nu_obs <- (11 / 13) * 10 * (1 - lambda)
  expect_equal(small$df, 1 / (1 / big$df + 1 / nu_obs))
  expect_lt(small$df, big$df)
  expect_equal(small$estimate, big$estimate)
  expect_error(pool_imputations(full, df_complete = -1), "df_complete")
})

test_that("tbl_schoenfeld_audit unwraps parsnip model_fit objects", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("censored")
  spec <- parsnip::set_engine(parsnip::set_mode(parsnip::proportional_hazards(), "censored regression"), "survival")
  fit <- suppressWarnings(parsnip::fit(spec, survival::Surv(time, status) ~ age + sex, data = survival::lung))
  expect_equal(tbl_schoenfeld_audit(fit)$term, tbl_schoenfeld_audit(fit$fit)$term)
})

test_that("prune_start_stop drops bad rows and closes rounding gaps", {
  df <- data.frame(
    id = c(1, 1, 1, 1, 2, 2, 3),
    tstart = c(0, 5 + 1e-8, 10, 12, 0, 3, 0),
    tstop = c(5, 10, 12, 15, 3, 3, NA),
    status = c(0, 0, 1, 0, 0, 0, 0)
  )
  out <- prune_start_stop(df)
  expect_equal(nrow(out), 4L)
  expect_equal(out$tstart[out$id == 1], c(0, 5, 10))
  expect_equal(attr(out, "pruned"),
               list(missing = 1L, nonpositive = 1L, after_event = 1L, gaps_closed = 1L))
  expect_true(validate_start_stop(out, warn_no_censoring = FALSE)$valid)
  expect_error(prune_start_stop(df, id = "nope"), "missing required")
})

test_that("tbl_missingness_patterns counts distinct patterns", {
  d <- data.frame(a = c(1, NA, 3, NA, 5, 6), b = c(1, NA, 3, NA, 5, NA), c = c(NA, 2, 3, 4, 5, 6))
  r <- tbl_missingness_patterns(d)
  expect_equal(sum(r$n), 6L)
  expect_equal(sum(r$percent), 100)
  expect_equal(r$n[1], 2L)
  expect_true(all(r$n[-1] <= r$n[1]))
  expect_equal(sort(r$n_missing_vars[r$n == 2L]), c(0L, 2L))
  expect_error(tbl_missingness_patterns(d, "zzz"), "not in")
})

test_that("plot_cumulative_incidence returns a ggplot for grouped and ungrouped input", {
  set.seed(3)
  d <- data.frame(time = rexp(200, 0.1), type = sample(0:2, 200, TRUE), arm = rep(c("A", "B"), 100))
  g <- tbl_cumulative_incidence(d, "time", "type", group_col = "arm", times = seq(0, 15, 3))
  expect_s3_class(plot_cumulative_incidence(g), "ggplot")
  expect_s3_class(plot_cumulative_incidence(g, facet = TRUE), "ggplot")
  u <- tbl_cumulative_incidence(d, "time", "type", times = seq(0, 15, 3))
  p <- plot_cumulative_incidence(u, facet = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(plot_cumulative_incidence(data.frame(a = 1)), "tbl_cumulative_incidence")
})
