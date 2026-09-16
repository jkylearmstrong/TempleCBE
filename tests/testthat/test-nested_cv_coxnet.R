test_that("nested_cv_coxnet tunes on inner resamples, refits, and scores each outer split", {
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 90)
  set.seed(6)
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 3)
  )
  metrics <- yardstick::metric_set(yardstick::brier_survival_integrated, yardstick::concordance_survival)

  res <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = c(0.5, 1), metrics = metrics,
    eval_time = c(3, 6, 9), nlambda = 6, cox.ties = "breslow"
  )
  expect_s3_class(res, "nested_cv_coxnet")
  expect_equal(nrow(res), 2)
  expect_named(res, c("id", "mixture", "penalty", ".metrics", ".coefs", ".inner"))
  expect_true(all(res$mixture %in% c(0.5, 1)))
  expect_setequal(res$.metrics[[1]]$.metric, c("brier_survival_integrated", "concordance_survival"))
  expect_equal(res$.coefs[[1]]$term, c("x1", "x2", "x3"))

  # The outer score comes from a model refit on the outer analysis set only.
  outer <- folds$splits[[1]]
  analysis <- rsample::analysis(outer)
  refit <- coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = analysis,
    mixture = res$mixture[1], penalty = res$penalty[1],
    path = res$.inner[[1]]$penalty[res$.inner[[1]]$mixture == res$mixture[1]],
    cox.ties = "breslow"
  )
  expect_equal(generics::tidy(refit)$estimate, res$.coefs[[1]]$estimate)

  one_se <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = c(0.5, 1), metrics = metrics, rule = "1se",
    eval_time = c(3, 6, 9), nlambda = 6, cox.ties = "breslow"
  )
  expect_true(all(one_se$penalty >= res$penalty[match(one_se$id, res$id)] | one_se$mixture != res$mixture))
})

test_that("collect_metrics summarizes nested results over outer splits", {
  skip_if_no_coxnet_deps()
  skip_if_not_installed("tune")
  d <- sim_counting(n = 90)
  set.seed(7)
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  res <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, subject_id = "subject_id",
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    eval_time = c(3, 6, 9), nlambda = 5, cox.ties = "breslow"
  )
  per_split <- tune::collect_metrics(res, summarize = FALSE)
  expect_equal(nrow(per_split), 2)
  summary <- tune::collect_metrics(res)
  expect_equal(summary$mean, mean(per_split$.estimate))
  expect_equal(summary$n, 2)
})

test_that("nested_cv_coxnet validates its inputs", {
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 30)
  expect_error(nested_cv_coxnet(d, survival::Surv(tstart, tstop, status) ~ x1 + x2), "nested_cv")
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(nested_cv_coxnet(folds, "not a model"), "formula or a recipe")
})
