test_that("calibration_table reproduces a hand-computed table", {
  y <- c(0, 0, 1, 1)
  p <- c(0.1, 0.1, 0.9, 0.9)
  tab <- calibration_table(y, p, bins = 2)
  expect_equal(tab$n, c(2L, 2L))
  expect_equal(tab$mean_pred, c(0.1, 0.9))
  expect_equal(tab$observed, c(0, 1))
  expect_true(all(tab$conf_low <= tab$observed & tab$observed <= tab$conf_high))
  m <- attr(tab, "metrics")
  expect_equal(m$brier, mean((p - y)^2))
  expect_equal(m$n, 4L)

  # Wilson interval for 1 of 4 at 95%
  z <- qnorm(0.975)
  one <- calibration_table(c(1, 0, 0, 0), rep(0.25, 4), bins = 2)
  centre <- (0.25 + z^2 / 8) / (1 + z^2 / 4)
  half <- z * sqrt(0.25 * 0.75 / 4 + z^2 / 64) / (1 + z^2 / 4)
  expect_equal(one$conf_low, centre - half)
  expect_equal(one$conf_high, centre + half)
})

test_that("calibration slope is near 1 for honest predictions and below 1 for overconfident ones", {
  set.seed(1)
  n <- 4000
  lp <- rnorm(n)
  y <- rbinom(n, 1, plogis(lp))
  honest <- attr(calibration_table(y, plogis(lp)), "metrics")
  over <- attr(calibration_table(y, plogis(3 * lp)), "metrics")
  expect_lt(abs(honest$slope - 1), 0.15)
  expect_lt(abs(honest$intercept), 0.15)
  expect_lt(over$slope, 0.5)
})

test_that("calibration_table takes factors, several models and the strategy", {
  set.seed(2)
  p <- runif(300)
  y <- factor(ifelse(rbinom(300, 1, p) == 1, "yes", "no"), levels = c("no", "yes"))
  tab <- calibration_table(y, list(a = p, b = 1 - p), bins = 5)
  expect_setequal(tab$model, c("a", "b"))
  expect_equal(sum(tab$n[tab$model == "a"]), 300L)
  # the event is the second level, so predictions p are calibrated and 1 - p are not
  expect_lt(attr(tab, "metrics")$brier[1], attr(tab, "metrics")$brier[2])
  first <- calibration_table(y, p, event_level = "first", bins = 5)
  expect_gt(attr(first, "metrics")$brier, attr(tab, "metrics")$brier[1])
  uni <- calibration_table(y, p, bins = 4, strategy = "uniform")
  expect_lte(nrow(uni), 4L)
})

test_that("calibration_table and decision_curve check their input", {
  expect_error(calibration_table(c(0, 1, 2), c(0.1, 0.2, 0.3)), "truth")
  expect_error(calibration_table(c(0, 1), c(0.1, 1.2)), "between 0 and 1")
  expect_error(calibration_table(c(0, 1), c(0.1, 0.2, 0.3)), "same length")
  expect_error(calibration_table(c(0, 1), c(0.1, 0.2), horizon = 5), "Surv")
  expect_error(calibration_table(c(0, 1), c(0.1, 0.2), bins = 1), "bins")
  expect_error(decision_curve(c(0, 1), c(0.1, 0.2), thresholds = c(0, 0.5)), "strictly between")
  skip_if_not_installed("survival")
  expect_error(calibration_table(survival::Surv(1:4, c(1, 0, 1, 0)), rep(0.5, 4)), "horizon")
  expect_error(calibration_table(survival::Surv(c(0, 1), c(2, 3), c(1, 0)), c(0.2, 0.3), horizon = 2), "right-censored")
})

test_that("decision_curve reproduces hand-computed net benefits", {
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.8, 0.3, 0.2)
  dc <- decision_curve(y, p, thresholds = c(0.25, 0.5))
  expect_s3_class(dc, "decision_curve")
  expect_equal(dc$net_benefit, c(0.5 - 0.25 * (0.25 / 0.75), 0.5))
  expect_equal(dc$treat_all, c(0.5 - 0.5 * (0.25 / 0.75), 0))
  expect_equal(dc$prop_above, c(0.75, 0.5))
  # above every prediction nobody is treated: net benefit 0
  expect_equal(decision_curve(y, p, thresholds = 0.95)$net_benefit, 0)
})

test_that("Surv outcomes without censoring give the binary calibration and decision curve", {
  skip_if_not_installed("survival")
  set.seed(3)
  n <- 200
  time <- rexp(n, 0.1)
  p <- runif(n)
  h <- 5
  ys <- survival::Surv(time, rep(1, n))
  yb <- as.numeric(time <= h)
  expect_equal(
    decision_curve(ys, p, horizon = h, thresholds = c(0.2, 0.5))$net_benefit,
    decision_curve(yb, p, thresholds = c(0.2, 0.5))$net_benefit
  )
  ts <- calibration_table(ys, p, horizon = h, bins = 4)
  tb <- calibration_table(yb, p, bins = 4)
  expect_equal(ts$observed, tb$observed)
  expect_true(all(is.na(attr(ts, "metrics")$slope)))
})

test_that("a censored Surv calibration uses the Kaplan-Meier risk", {
  skip_if_not_installed("survival")
  time <- c(2, 4, 6, 8, 3, 5, 7, 9)
  event <- c(1, 0, 1, 0, 1, 1, 0, 1)
  p <- rep(0.4, 8)
  tab <- calibration_table(survival::Surv(time, event), p, horizon = 5, bins = 2)
  sf <- summary(survival::survfit(survival::Surv(time, event) ~ 1), times = 5)
  expect_equal(tab$observed, 1 - sf$surv)
})

test_that("plot_calibration and plot_decision_curve return plots", {
  set.seed(4)
  p <- runif(200)
  y <- rbinom(200, 1, p)
  g <- plot_calibration(y, list(a = p, b = p^2), smooth = TRUE)
  expect_s3_class(g, "ggplot")
  expect_s3_class(ggplot2::ggplot_build(g), "ggplot_built")
  expect_match(plot_calibration(y, p)$labels$subtitle, "slope")
  d <- plot_decision_curve(y, pred = list(a = p, b = p^2))
  expect_s3_class(ggplot2::ggplot_build(d), "ggplot_built")
  expect_s3_class(plot_decision_curve(decision_curve(y, p)), "ggplot")
})
