# Tests for the round-2 survival-core fixes (items 1-10) of
# TempleCBE-gaps-round2-2026-11-12.md, section A.
#
# Local simulators are defined here (not borrowed from other test files:
# testthat does not share objects defined in another test file).

# Readmission intervals with deaths, as in the B1 tests: a subject who dies
# has their intervals cut at the death time, with death on the last one.
sim_r2_readmission_death <- function(n = 120, seed = 1) {
  set.seed(seed)
  subj <- data.frame(id = seq_len(n), age = stats::rnorm(n), end = 120)
  ev <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- stats::rpois(1, exp(0.3 * subj$age[i]) * 1.2)
    if (k == 0) return(NULL)
    data.frame(id = i, time = sort(stats::runif(k, 1, 120)))
  }))
  long <- recurrent_intervals(ev, subj)
  long$age <- subj$age[long$id]
  long$death <- 0L
  died <- stats::rbinom(n, 1, stats::plogis(-1 + 0.5 * subj$age)) == 1
  death_time <- stats::runif(n, 20, 120)
  for (i in which(died)) {
    rows_i <- which(long$id == i)
    cut <- truncate_start_stop(long[rows_i, , drop = FALSE], death_time[i])
    long <- long[-rows_i, , drop = FALSE]
    cut$death <- 0L
    cut$death[nrow(cut)] <- 1L
    long <- rbind(long, cut)
  }
  long[order(long$id, long$tstart), , drop = FALSE]
}

# Recurrent-event intervals for recurrent_cox() validation tests.
sim_r2_recurrent <- function(n = 300, beta = 0.4, seed = 1) {
  set.seed(seed)
  subj <- data.frame(id = seq_len(n), x = stats::rnorm(n), end = 120)
  ev <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- stats::rpois(1, exp(beta * subj$x[i]) * 1.2)
    if (k == 0) return(NULL)
    data.frame(id = i, time = sort(stats::runif(k, 1, 120)))
  }))
  long <- recurrent_intervals(ev, subj)
  long$x <- subj$x[long$id]
  long
}

test_that("print.templecbe_run_comparison does not recurse on a non-reproducible comparison (item 1)", {
  r1 <- tibble::tibble(model = c("a", "b"), IBS = c(0.20, NA_real_), lambda = c(0.1, 0.2))
  r2 <- tibble::tibble(model = c("a", "b"), IBS = c(0.20, 0.30), lambda = c(0.1, 0.2))
  out <- compare_runs(r1, r2)
  expect_false(attr(out, "comparison_summary")$all_reproducible)
  printed <- capture.output(print(out))
  # The header is printed exactly once (before the fix it was printed ~475
  # times before the C-stack error); the discrepant model is listed.
  expect_equal(sum(grepl("TempleCBE Cross-Platform Run Comparison", printed)), 1L)
  expect_true(any(grepl("exceeded tolerance", printed)))
  expect_true(any(grepl("b", printed)))
})

test_that("compare_tuning_runs(match = 'nearest') returns NA run-2 sides for fully disjoint formulas (item 2)", {
  t1 <- tibble::tibble(alpha = c(0, 0.5), formula = c("x1", "x2"), IBS = c(0.2, 0.25))
  t2 <- tibble::tibble(alpha = 0.1, formula = "x3", IBS = 0.21)
  expect_warning(
    out <- compare_tuning_runs(t1, t2, match = "nearest"),
    "no match"
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$mean_ibs_run2)))
  expect_false(attr(out, "comparison_summary")$all_reproducible)
  expect_equal(attr(out, "comparison_summary")$n_unassessable, 2L)
  expect_true(is.na(attr(out, "comparison_summary")$rank_correlation))
})

test_that("compare_runs returns a comparison (not a cor() error) when both runs' IBS are all NA (item 3)", {
  r1 <- tibble::tibble(model = c("a", "b"), IBS = c(NA_real_, NA_real_), lambda = c(0.1, 0.2))
  r2 <- tibble::tibble(model = c("a", "b"), IBS = c(NA_real_, NA_real_), lambda = c(0.1, 0.2))
  out <- compare_runs(r1, r2)
  expect_false(out$reproducible[1])
  expect_false(out$reproducible[2])
  s <- attr(out, "comparison_summary")
  expect_equal(s$n_unassessable, 2L)
  expect_true(is.na(s$max_ibs_diff))
  expect_true(is.na(s$rank_correlation))
  # ... and the all-NA comparison can be printed (items 1 + 3 together)
  printed <- capture.output(print(out))
  expect_equal(sum(grepl("TempleCBE Cross-Platform Run Comparison", printed)), 1L)
})

test_that("hazard_ratio_at_time and predict error at t = 0 with time_fn = 'log' (item 4)", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 200
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  expect_error(hazard_ratio_at_time(fit, "age", times = 0), "non-finite")
  expect_error(hazard_ratio_at_time(fit, "age", times = c(0, 10)), "non-finite")
  # t > 0 is unaffected
  hr <- hazard_ratio_at_time(fit, "age", times = c(1, 10))
  expect_true(all(is.finite(hr$hazard_ratio)))
  expect_error(
    predict(fit, d[1:3, ], type = "linear_pred", eval_time = 0),
    "non-finite"
  )
  # survival at t = 0 is still 1 (no event time is <= 0); only the linear
  # predictor is undefined there
  sv <- predict(fit, d[1:3, ], type = "survival", eval_time = 0)
  expect_true(all(vapply(sv$.pred, function(p) all(p$.pred_survival == 1), logical(1))))
  # with time_fn = "identity", t = 0 is finite and works
  fit_id <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age", time_fn = "identity")
  hr0 <- hazard_ratio_at_time(fit_id, "age", times = 0)
  expect_true(all(is.finite(hr0$hazard_ratio)))
  lp0 <- predict(fit_id, d[1:3, ], type = "linear_pred", eval_time = 0)
  expect_true(all(is.finite(lp0$.pred_linear_pred)))
})

test_that("as_tuning_results overwrites a non-Brier .metric when IBS comes from a Brier column (item 5)", {
  # .metric = "concordance" with an `ibs` column: the old output claimed
  # concordance next to a Brier score
  expect_warning(
    out <- as_tuning_results(data.frame(alpha = 0.5, ibs = 0.2, .metric = "concordance")),
    "non-Brier"
  )
  expect_equal(out$IBS, 0.2)
  expect_equal(out$.metric, "brier_survival_integrated")
  # same when the IBS column is already present
  expect_warning(
    out2 <- as_tuning_results(data.frame(alpha = 0.5, IBS = 0.2, .metric = "concordance")),
    "non-Brier"
  )
  expect_equal(out2$.metric, "brier_survival_integrated")
  # a `brier_score` source gets the generic "brier" name
  expect_warning(
    out3 <- as_tuning_results(data.frame(alpha = 0.5, brier_score = 0.2, .metric = "concordance")),
    "non-Brier"
  )
  expect_equal(out3$.metric, "brier")
  # a .metric that already names a Brier metric is left alone, silently
  expect_warning(
    out4 <- as_tuning_results(data.frame(alpha = 0.5, ibs = 0.2, .metric = "brier_survival_integrated")),
    NA
  )
  expect_equal(out4$.metric, "brier_survival_integrated")
})

test_that("cox_time_varying honours ties via ... and warns on model/x/y (item 6)", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 200
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
  expect_s3_class(
    cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age", ties = "breslow"),
    "cox_time_varying"
  )
  expect_s3_class(
    cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age", ties = "efron"),
    "cox_time_varying"
  )
  # model/x/y are set internally: warned about and ignored, not a cryptic
  # "formal argument matched by multiple actual arguments" error
  expect_warning(
    fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age", model = TRUE),
    "sets `model`"
  )
  expect_s3_class(fit, "cox_time_varying")
  expect_error(
    cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age", tt = 1),
    "`tt` is set"
  )
})

test_that("gap_time_concordance rejects infinite linear predictors (item 7)", {
  skip_if_not_installed("survival")
  skip_if_not_installed("yardstick")
  g <- surv_gap_truth(
    survival::Surv(c(0, 0, 0), c(5, 6, 7), c(1, 1, 0)),
    subject_id = c(1, 2, 3)
  )
  expect_error(gap_time_concordance(g, c(0.1, Inf, 0.3)), "finite")
  expect_error(gap_time_concordance(g, c(0.1, -Inf, 0.3)), "finite")
  expect_error(gap_time_concordance(g, c(0.1, NA_real_, 0.3)), "finite")
  expect_true(is.finite(gap_time_concordance(g, c(0.1, 0.2, 0.3))))
})

test_that("joint_readmission_death rejects a factor readmission predictor at fit time (item 8)", {
  skip_if_not_installed("survival")
  long <- sim_r2_readmission_death(n = 60, seed = 1)
  long$grp <- factor(sample(c("a", "b"), nrow(long), replace = TRUE))
  expect_error(joint_readmission_death(long, ~ age + grp, ~ age), "must be numeric")
  # the death side already rejected factors; the readmission side now matches
  expect_error(joint_readmission_death(long, ~ age, ~ grp), "must be numeric")
  # numeric predictors still fit
  expect_s3_class(joint_readmission_death(long, ~ age, ~ age), "joint_readmission_death")
})

test_that("recurrent_cox rejects Inf max_events / min_stratum_events (item 9)", {
  skip_if_not_installed("survival")
  d <- sim_r2_recurrent()
  expect_error(recurrent_cox(d, ~ x, model = "pwp_total", max_events = Inf), "max_events")
  expect_error(recurrent_cox(d, ~ x, model = "pwp_total", min_stratum_events = Inf), "min_stratum_events")
  expect_error(recurrent_cox(d, ~ x, model = "pwp_total", max_events = 1.5), "max_events")
  expect_error(recurrent_cox(d, ~ x, model = "pwp_total", min_stratum_events = NA_real_), "min_stratum_events")
  expect_s3_class(recurrent_cox(d, ~ x, model = "pwp_total", max_events = 2), "recurrent_cox")
})

test_that("tune_race_survival_checkpointed warns when it falls back to the first-split fingerprint (item 10)", {
  skip_if_not_installed("openssl")
  calls <- 0L
  testthat::local_mocked_bindings(
    compute_tidymodels_split_fingerprint = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) stop("split 2 fingerprint failed")
      "fp-stub"
    }
  )
  resamples <- tibble::tibble(splits = list(1, 2))
  ckpt_dir <- file.path(tempdir(), "r2-race-fallback")
  if (dir.exists(ckpt_dir)) unlink(ckpt_dir, recursive = TRUE)
  # the per-split computation failed on split 2, so the fallback (and its
  # warning) must have run; the race itself cannot run here, which is fine
  w <- expect_warning(
    tryCatch(
      tune_race_survival_checkpointed(list(), resamples, checkpoint_dir = ckpt_dir),
      error = function(e) e
    ),
    "first split only"
  )
  expect_match(conditionMessage(w), "split 2 fingerprint failed")
  expect_false(file.exists(file.path(ckpt_dir, "race_survival_result.rds")))
})
