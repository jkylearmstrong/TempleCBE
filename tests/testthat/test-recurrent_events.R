recurrent_sim <- function(n = 300, beta = 0.4, seed = 1) {
  set.seed(seed)
  subj <- data.frame(id = seq_len(n), x = rnorm(n), end = 120)
  ev <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- rpois(1, exp(beta * subj$x[i]) * 1.2)
    if (k == 0) return(NULL)
    data.frame(id = i, time = sort(runif(k, 1, 120)))
  }))
  long <- recurrent_intervals(ev, subj)
  long$x <- subj$x[long$id]
  long
}

test_that("recurrent_intervals builds the (tstart, tstop, status) rows of a hand example", {
  events <- data.frame(id = c(1, 1, 3), time = c(12, 40, 25))
  followup <- data.frame(id = 1:3, end = c(90, 60, 30))
  out <- recurrent_intervals(events, followup)
  expect_named(out, c("id", "tstart", "tstop", "status", "event_number"))
  expect_equal(out$id, c(1, 1, 1, 2, 3, 3))
  expect_equal(out$tstart, c(0, 12, 40, 0, 0, 25))
  expect_equal(out$tstop, c(12, 40, 90, 60, 25, 30))
  expect_equal(out$status, c(1, 1, 0, 0, 1, 0))
  expect_equal(out$event_number, c(1, 2, 3, 1, 1, 2))
  # no overlaps, no rows after-event problems for recurrent data
  expect_true(validate_start_stop(out, recurrent = TRUE, warn_no_censoring = FALSE)$valid)
})

test_that("recurrent_intervals ends on an event at the end of follow-up and reports what it drops", {
  events <- data.frame(id = c(1, 1), time = c(10, 30))
  out <- recurrent_intervals(events, data.frame(id = 1, end = 30))
  expect_equal(out$status, c(1, 1))
  expect_equal(out$tstop, c(10, 30))

  expect_warning(
    expect_warning(
      out <- recurrent_intervals(
        data.frame(id = c(1, 1, 1, 1), time = c(0, 10, 10, 200)),
        data.frame(id = 1, end = 100)
      ),
      "after the end of follow-up"
    ),
    "tied"
  )
  expect_equal(out$tstop, c(10, 100))

  expect_warning(
    out <- recurrent_intervals(data.frame(id = c(1, 1), time = c(10, 11)), data.frame(id = 1, end = 50), min_gap = 2),
    "min_gap"
  )
  expect_equal(out$tstop, c(10, 50))

  expect_error(recurrent_intervals(data.frame(id = 9, time = 1), data.frame(id = 1, end = 5)), "no follow-up row")
  expect_error(recurrent_intervals(data.frame(id = 1, time = 1), data.frame(id = c(1, 1), end = c(5, 6))), "one row per subject")
})

test_that("truncate_start_stop cuts at the window and censors the cut interval", {
  d <- data.frame(id = c(1, 1, 2), tstart = c(0, 20, 0), tstop = c(20, 70, 50), status = c(1, 1, 0))
  out <- truncate_start_stop(d, 30)
  expect_equal(out$tstop, c(20, 30, 30))
  expect_equal(out$status, c(1, 0, 0))
  expect_equal(nrow(truncate_start_stop(d, 20)), 2L) # the interval that starts at the window is dropped
  expect_error(truncate_start_stop(d, -1), "positive")
})

test_that("validate_start_stop and prune_start_stop take recurrent events", {
  d <- data.frame(id = c(1, 1, 1), tstart = c(0, 10, 20), tstop = c(10, 20, 30), status = c(1, 1, 0))
  expect_false(validate_start_stop(d, warn_no_censoring = FALSE)$valid)
  v <- validate_start_stop(d, recurrent = TRUE, warn_no_censoring = FALSE)
  expect_true(v$valid)
  expect_equal(v$n_events_before_last, 2L)
  expect_equal(nrow(prune_start_stop(d)), 1L)
  expect_equal(nrow(prune_start_stop(d, recurrent = TRUE)), 3L)
  expect_error(validate_start_stop(d, recurrent = NA), "TRUE or FALSE")
})

test_that("surv_subject_truth(recurrent = 'first') scores the time to the first event", {
  skip_if_not_installed("survival")
  y <- survival::Surv(c(0, 10, 20, 0, 0, 7), c(10, 20, 30, 15, 6, 9), c(1, 1, 0, 0, 1, 0))
  id <- c(1, 1, 1, 2, 3, 3)
  expect_error(surv_subject_truth(y, id), "recurrent")
  out <- surv_subject_truth(y, id, recurrent = "first")
  expect_equal(out$.subject_id, 1:3)
  expect_equal(unname(unclass(out$.truth)[, "time"]), c(10, 15, 6))
  expect_equal(unname(unclass(out$.truth)[, "status"]), c(1, 0, 1))
})

test_that("recurrent_cox 'ag' reproduces survival::coxph with a subject cluster", {
  skip_if_not_installed("survival")
  d <- recurrent_sim()
  fit <- recurrent_cox(d, ~ x, model = "ag")
  ref <- survival::coxph(survival::Surv(tstart, tstop, status) ~ x + cluster(id), data = d, ties = "efron")
  expect_s3_class(fit, "recurrent_cox")
  expect_equal(fit$terms$estimate, unname(coef(ref)))
  expect_equal(fit$terms$std_error, unname(sqrt(diag(vcov(ref)))))
  expect_equal(fit$terms$hr, exp(fit$terms$estimate))
  expect_equal(fit$n_events, sum(d$status))
  expect_equal(fit$n_subjects, 300L)
  expect_equal(tidy(fit), fit$terms)
  # the simulated log hazard ratio is 0.4
  expect_lt(abs(fit$terms$estimate - 0.4), 0.15)
  expect_output(print(fit), "Andersen-Gill")
  # predictors as a character vector are the same fit
  expect_equal(recurrent_cox(d, "x", model = "ag")$terms, fit$terms)
})

test_that("recurrent_cox fits PWP, WLW and frailty and stratifies by event number", {
  skip_if_not_installed("survival")
  d <- recurrent_sim()
  for (m in c("pwp_total", "pwp_gap", "wlw", "frailty")) {
    fit <- recurrent_cox(d, ~ x, model = m)
    expect_s3_class(fit, "recurrent_cox")
    expect_equal(nrow(fit$terms), 1L)
    expect_true(is.finite(fit$terms$std_error) && fit$terms$std_error > 0)
    expect_lt(abs(fit$terms$estimate - 0.4), 0.25)
  }
  pwp <- recurrent_cox(d, ~ x, model = "pwp_total", max_events = 2)
  expect_equal(length(pwp$strata_events), 2L)
  expect_equal(sum(pwp$strata_events), pwp$n_events)
  ref <- survival::coxph(survival::Surv(tstart, tstop, status) ~ x + strata(pmin(
    1 + ave(status, id, FUN = function(v) c(0, cumsum(v)[-length(v)])), 2)) + cluster(id), data = d)
  expect_equal(pwp$terms$estimate, unname(coef(ref)))
  expect_true(is.finite(recurrent_cox(d, ~ x, model = "frailty")$frailty_variance))
})

test_that("recurrent_cox rejects bad input", {
  skip_if_not_installed("survival")
  d <- data.frame(id = c(1, 1), tstart = c(0, 5), tstop = c(5, 3), status = c(1, 0), x = 1:2)
  expect_error(recurrent_cox(d, ~ x), "Invalid start/stop")
  none <- data.frame(id = 1:3, tstart = 0, tstop = c(5, 6, 7), status = 0, x = 1:3)
  expect_error(recurrent_cox(none, ~ x), "no events")
  expect_error(recurrent_cox(recurrent_sim(50), ~ x, conf_level = 2), "conf_level")
  expect_error(recurrent_cox(recurrent_sim(50), 1), "predictors")
})

test_that("recurrent_cox_windows fits one model per 30/60/90-day window", {
  skip_if_not_installed("survival")
  d <- recurrent_sim(400)
  out <- recurrent_cox_windows(d, ~ x, windows = c(30, 60, 90), model = "ag")
  expect_equal(out$window, c(30, 60, 90))
  expect_equal(names(attr(out, "fits")), c("30", "60", "90"))
  expect_true(all(diff(out$n_events) > 0))
  cut <- truncate_start_stop(d, 60)
  expect_equal(out$n_events[2], sum(cut$status))
  expect_equal(out$estimate[2], recurrent_cox(cut, ~ x)$terms$estimate)
  expect_error(recurrent_cox_windows(d, ~ x, windows = c(-1, 30)), "positive")
})
