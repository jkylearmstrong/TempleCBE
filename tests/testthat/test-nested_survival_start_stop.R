# Item 21 (0.8.1 follow-up): nested_survival_run() on genuine multi-interval
# start-stop data.
#
# The only end-to-end nested_survival_run() test used rossi_data() reshaped to
# a single interval per subject (tstart = 0 for every row), so it never
# exercised the counting-process path that blew up in subject_keys():
# stats::ave() over (subject, start, stop) built the full interaction of
# distinct starts x distinct stops, which exhausts memory on real start-stop
# data. nested_survival_run() reaches that code through glmnet_IBS(), so it
# inherits the subject_keys() fix -- this test proves the run now completes
# on multi-interval data instead of blowing up.
#
# Dataset: small synthetic counting-process data from the shared
# sim_counting() helper (tests/testthat/helper-survival-data.R) -- 36
# subjects, 70 rows, 26 subjects with more than one interval, 68 distinct
# stop times, 18 events / 18 censored subjects. A pbcseq subset built via
# tidy_tmerge_cox() would exercise the same path, but the synthetic set keeps
# the test self-contained and fast. Kept deliberately small and cheap:
# outer_v = 2, inner_v = 2, a single alpha, a single formula, and
# internal_folds = 2, which runs in ~15-20 s (internal_folds = 3 roughly
# doubles that with no change to the result).

test_that("nested_survival_run() completes on multi-interval start-stop data", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "ggplot2", "jsonlite")) {
    skip_if_not_installed(pkg)
  }

  d <- sim_counting(n = 36, seed = 11)

  # Genuinely multi-interval start-stop data, not one row per subject: the
  # rossi-based test has tstart == 0 on every row, which cannot stress
  # subject_keys().
  expect_gt(nrow(d), length(unique(d$subject_id)))
  expect_true(any(table(d$subject_id) > 1))
  expect_gt(length(unique(d$tstart)), 1L)
  expect_gt(length(unique(d$tstop)), 1L)
  expect_true(any(d$status == 1) && any(d$status == 0))

  run_dir <- file.path(tempdir(), paste0("nested_ss_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)

  run <- nested_survival_run(
    data = d,
    id = "subject_id",
    start = "tstart",
    stop = "tstop",
    status = "status",
    domain = "SimStartStop",
    run_dir = run_dir,
    feature_names = c("x1", "x2", "x3"),
    formulas = "x1 + x2 + x3",
    outer_v = 2,
    inner_v = 2,
    alphas = 0.5,
    top_n = 1,
    internal_folds = 2,
    cox.ties = "breslow",
    seed = 67025
  )

  # The run completes (the point of this test) and returns the expected object.
  expect_s3_class(run, "cbe_nested_survival_run")
  expect_equal(run$status, "completed")
  expect_false(run$resumed)
  expect_true(file.exists(file.path(run_dir, "nested_survival_run.rds")))

  # One outer score per outer fold, with finite integrated Brier scores.
  expect_equal(nrow(run$outer_scores), 2L)
  expect_true(all(is.finite(run$outer_scores$IBS)))
  expect_true(all(run$outer_scores$IBS >= 0 & run$outer_scores$IBS <= 1))
  expect_true(nrow(run$tune_results) > 0L)
  expect_true(nrow(run$candidates) > 0L)

  # S3 summary works and describes the same 36-subject start-stop run.
  s_run <- summary(run)
  expect_s3_class(s_run, "summary.cbe_nested_survival_run")
  expect_equal(s_run$overview$domain, "SimStartStop")
  expect_equal(s_run$overview$n_subjects, 36L)
  expect_equal(s_run$overview$n_outer_folds, 2L)
  expect_true(isTRUE(s_run$overview$has_censoring))
  expect_true(is.finite(s_run$overview$median_outer_ibs))
})
