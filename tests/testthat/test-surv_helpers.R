test_that("surv_subject_truth collapses start/stop rows to one right-censored row per subject", {
  skip_if_not_installed("survival")
  y <- survival::Surv(c(0, 5, 0, 0, 4), c(5, 9, 6, 4, 10), c(0, 1, 0, 0, 0))
  out <- surv_subject_truth(y, subject_id = c(1, 1, 2, 3, 3))

  expect_named(out, c(".subject_id", ".entry", ".truth"))
  expect_equal(out$.subject_id, c(1, 2, 3))
  expect_identical(attr(out$.truth, "type"), "right")
  expect_equal(unname(unclass(out$.truth)[, "time"]), c(9, 6, 10))
  expect_equal(unname(unclass(out$.truth)[, "status"]), c(1, 0, 0))
  expect_equal(out$.entry, c(0, 0, 0))
})

test_that("surv_subject_truth sorts rows, handles right-censored data, and validates subjects", {
  skip_if_not_installed("survival")
  unsorted <- surv_subject_truth(survival::Surv(c(5, 0), c(9, 5), c(1, 0)), c("a", "a"))
  expect_equal(unname(unclass(unsorted$.truth)[, "status"]), 1)

  right <- surv_subject_truth(survival::Surv(c(3, 1), c(1, 0)))
  expect_equal(right$.subject_id, 1:2)

  expect_error(
    surv_subject_truth(survival::Surv(c(0, 5), c(5, 9), c(1, 0)), c(1, 1)),
    "event before their last interval"
  )
  expect_error(surv_subject_truth(survival::Surv(c(0, 3), c(5, 9), c(0, 0)), c(1, 1)), "overlap")
  expect_error(surv_subject_truth(survival::Surv(c(0, 5), c(5, 9), c(0, 1))), "subject_id")
  expect_error(surv_subject_truth(survival::Surv(c(0, 5), c(5, 9), c(0, 1)), 1), "one value per")
  expect_error(surv_subject_truth(1:3), "Surv")
})

test_that("subject_keys separates bootstrap copies of a subject and leaves other ids alone", {
  skip_if_not_installed("survival")
  y <- survival::Surv(c(0, 5, 0), c(5, 9, 6), c(0, 1, 0))
  expect_identical(subject_keys(y, c(1, 1, 2)), c(1, 1, 2))

  # Subject 1 drawn twice by a bootstrap: its two intervals appear twice.
  boot <- survival::Surv(c(0, 5, 0, 5, 0), c(5, 9, 5, 9, 6), c(0, 1, 0, 1, 0))
  keys <- subject_keys(boot, c(1, 1, 1, 1, 2))
  expect_identical(keys, c("1#1", "1#1", "1#2", "1#2", "2#1"))
  truth <- surv_subject_truth(boot, keys)
  expect_equal(nrow(truth), 3)
  expect_equal(unname(unclass(truth$.truth)[, "status"]), c(1, 1, 0))
})

test_that("censoring_km steps at censoring times, with a left limit and a floor", {
  skip_if_not_installed("survival")
  # Censored at 2 and 6; events at 4 and 8.
  cens <- censoring_km(survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1)))
  expect_s3_class(cens, "censoring_km")
  expect_equal(predict(cens, c(1, 2, 3, 6)), c(1, 0.75, 0.75, 0.375))
  expect_equal(predict(cens, c(2, 6), left = TRUE), c(1, 0.75))
  expect_equal(predict(cens, 6, trunc = 0.5), 0.5)
  expect_error(censoring_km(survival::Surv(c(0, 1), c(1, 2), c(0, 1))), "surv_subject_truth")
})

test_that("graf_weights and add_graf_weights reproduce a hand-computed Brier score", {
  skip_if_not_installed("survival")
  skip_if_not_installed("yardstick")
  # G(t) = 1 before 2, 3/4 on [2, 6), 3/8 from 6.
  cens <- censoring_km(survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1)))
  # Subject 1: event at 5. Subject 2: censored at 7. Subject 3: censored at 2.5.
  truth <- survival::Surv(c(5, 7, 2.5), c(1, 0, 0))

  w <- graf_weights(truth, eval_time = c(3, 6), censoring = cens, trunc = 0)
  # 1: at risk at 3 -> 1/G(3); event by 6 -> 1/G(5-). 2: at risk at both.
  # 3: censored before both -> 0.
  expect_equal(unname(w), rbind(c(4 / 3, 4 / 3), c(4 / 3, 8 / 3), c(0, 0)))

  scored <- tibble::tibble(
    .truth = truth,
    .pred = list(
      tibble::tibble(.eval_time = c(3, 6), .pred_survival = c(0.9, 0.5)),
      tibble::tibble(.eval_time = c(3, 6), .pred_survival = c(0.8, 0.6)),
      tibble::tibble(.eval_time = c(3, 6), .pred_survival = c(0.7, 0.2))
    )
  )
  scored <- add_graf_weights(scored, censoring = cens, trunc = 0)
  expect_equal(scored$.pred[[2]]$.weight_censored, c(4 / 3, 8 / 3))

  bs <- yardstick::brier_survival(scored, truth = .truth, .pred)
  # t = 6: subject 1 had the event -> 0.5^2 * 4/3; subject 2 still at risk -> 0.4^2 * 8/3.
  expect_equal(bs$.estimate[bs$.eval_time == 6], (0.25 * 4 / 3 + 0.16 * 8 / 3) / 3)

  expect_error(add_graf_weights(scored, estimate = "nope", censoring = cens), "columns")
  expect_error(graf_weights(truth, c(3, 6), censoring = "x"), "censoring_km")
})

test_that("breslow_cumhaz matches survival's Breslow estimate on counting-process data", {
  skip_if_not_installed("survival")
  d <- data.frame(
    start = c(0, 5, 0, 0, 3, 0, 2), stop = c(5, 9, 4, 6, 8, 8, 9),
    status = c(0, 1, 1, 0, 1, 1, 0), lp = c(0.2, 0.3, -0.1, 0, 0.5, -0.4, 0.1)
  )
  ref <- survival::coxph(survival::Surv(start, stop, status) ~ offset(lp), data = d, ties = "breslow")
  ref_fit <- survival::survfit(ref, newdata = data.frame(lp = 0), se.fit = FALSE)

  bh <- breslow_cumhaz(cbind(d$lp, 2 * d$lp), d$start, d$stop, d$status)
  expect_equal(cumhaz_at(bh, ref_fit$time)[, 1], ref_fit$cumhaz)

  ref2 <- survival::coxph(survival::Surv(start, stop, status) ~ offset(2 * lp), data = d, ties = "breslow")
  ref2_fit <- survival::survfit(ref2, newdata = data.frame(lp = 0), se.fit = FALSE)
  expect_equal(cumhaz_at(bh, ref2_fit$time)[, 2], ref2_fit$cumhaz)
})

test_that("predict_subject_survival integrates the hazard over each subject's covariate path", {
  bh <- list(time = c(2, 4, 6), cumhaz = matrix(c(0.1, 0.3, 0.6), ncol = 1))
  # Subject a: (0, 3] at lp 0, then (3, 5] at lp log(2). Subject b: enters at 2, lp 0.
  lp <- matrix(c(0, log(2), 0), ncol = 1)
  pred <- predict_subject_survival(
    lp, start = c(0, 3, 2), stop = c(3, 5, 6), id = c("a", "a", "b"),
    eval_time = c(1, 4, 7), bh = bh, covariates = "path"
  )
  expect_equal(pred$id, c("a", "b"))
  # a at 4: H(3) * 1 + (H(4) - H(3)) * 2 = 0.1 + 0.4. At 7: 0.1 + (0.6 - 0.1) * 2.
  expect_equal(pred$surv[1, , 1], exp(-c(0, 0.5, 1.1)))
  # b is carried back to time 0.
  expect_equal(pred$surv[2, , 1], exp(-c(0, 0.3, 0.6)))

  baseline <- predict_subject_survival(
    lp, start = c(0, 3, 2), stop = c(3, 5, 6), id = c("a", "a", "b"),
    eval_time = c(1, 4, 7), bh = bh, covariates = "baseline"
  )
  expect_equal(baseline$surv[1, , 1], exp(-c(0, 0.3, 0.6)))
})
