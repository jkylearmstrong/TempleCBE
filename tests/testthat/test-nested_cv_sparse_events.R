sparse_folds <- function(n_events) {
  d <- sim_counting(n = 90)
  ev <- which(d$status == 1)
  d$status[setdiff(ev, ev[seq_len(n_events)])] <- 0
  set.seed(6)
  rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 3)
  )
}

run_sparse <- function(folds) {
  metrics <- yardstick::metric_set(yardstick::brier_survival_integrated, yardstick::concordance_survival)
  nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = 1, metrics = metrics,
    eval_time = c(3, 6, 9), nlambda = 6, cox.ties = "breslow"
  )
}

test_that("nested_cv_coxnet() with no events at all stops with a plain message", {
  skip_if_no_coxnet_deps()
  expect_error(suppressWarnings(run_sparse(sparse_folds(0))), "no events")
})

test_that("nested_cv_coxnet() with only two events runs to the end instead of crashing in glmnet", {
  skip_if_no_coxnet_deps()
  res <- suppressWarnings(suppressMessages(run_sparse(sparse_folds(2))))
  expect_s3_class(res, "nested_cv_coxnet")
  expect_equal(nrow(res), 2L)
})
