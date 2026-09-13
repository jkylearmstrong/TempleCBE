test_that("cv_coxnet groups folds by subject, scores with yardstick, and picks lambda.min and lambda.1se", {
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  set.seed(2)
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    data = d, subject_id = "subject_id", mixture = c(0.5, 1), v = 3,
    eval_time = c(3, 6, 9), nlambda = 8, cox.ties = "breslow"
  )
  expect_s3_class(cv, "cv_coxnet")

  for (split in cv$resamples$splits) {
    expect_length(intersect(rsample::analysis(split)$subject_id, rsample::assessment(split)$subject_id), 0)
  }
  expect_setequal(
    unique(cv$metrics$.metric),
    c("brier_survival_integrated", "concordance_survival", "brier_survival", "roc_auc_survival")
  )
  expect_true(all(c("mixture", "penalty", ".metric", ".eval_time", "mean", "n", "std_err") %in% names(cv$metrics)))
  expect_equal(nrow(cv$fold_metrics) / length(unique(cv$fold_metrics$id)), nrow(cv$metrics))

  ibs <- cv$metrics[cv$metrics$.metric == "brier_survival_integrated", ]
  expect_equal(cv$metric, "brier_survival_integrated")
  expect_equal(min(ibs$mean), cv$best$mean)
  expect_equal(cv$lambda_min, cv$best$penalty)
  expect_gte(cv$lambda_1se, cv$lambda_min)
  expect_true(cv$mixture %in% c(0.5, 1))
  expect_setequal(cv$best_by_mixture$mixture, c(0.5, 1))

  expect_equal(nrow(predict(cv, d[1:3, ], type = "survival", eval_time = 6)), 3)
  expect_equal(generics::tidy(cv)$term, c("x1", "x2", "x3"))
  expect_equal(generics::tidy(cv, penalty = "lambda.1se")$penalty[1], cv$lambda_1se)
  expect_s3_class(ggplot2::autoplot(cv), "ggplot")
  expect_output(print(cv), "lambda.min")
})

test_that("select_coxnet_penalty picks the best penalty and the largest within one standard error", {
  summary <- tibble::tibble(
    mixture = 1, penalty = c(0.1, 0.2, 0.4, 0.8), .metric = "m", .estimator = "standard",
    .eval_time = NA_real_, n = 3,
    mean = c(0.30, 0.20, 0.22, 0.40), std_err = c(0.01, 0.03, 0.01, 0.01)
  )
  low <- select_coxnet_penalty(summary, "m", "minimize")
  expect_equal(c(low$lambda_min, low$lambda_1se), c(0.2, 0.4))

  summary$mean <- c(0.70, 0.80, 0.78, 0.60)
  high <- select_coxnet_penalty(summary, "m", "maximize")
  expect_equal(c(high$lambda_min, high$lambda_1se), c(0.2, 0.4))

  expect_error(select_coxnet_penalty(summary, "other", "minimize"), "No resample")
})

test_that("cv_coxnet takes a recipe with an id role, site-grouped folds, and one metric", {
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  d$surv <- survival::Surv(d$tstart, d$tstop, d$status)
  rec <- recipes::recipe(surv ~ x1 + x2 + x3 + subject_id + site, data = d)
  rec <- recipes::update_role(rec, "subject_id", new_role = "id")
  rec <- recipes::update_role(rec, "site", new_role = "site")
  rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())

  set.seed(3)
  cv <- cv_coxnet(
    rec, d, group = "site", v = 4, eval_time = c(3, 6, 9),
    metrics = yardstick::metric_set(yardstick::concordance_survival),
    nlambda = 6, cox.ties = "breslow"
  )
  expect_equal(cv$metric, "concordance_survival")
  expect_equal(cv$direction, "maximize")
  for (split in cv$resamples$splits) {
    expect_length(intersect(rsample::analysis(split)$site, rsample::assessment(split)$site), 0)
  }

  bad <- recipes::update_role(recipes::recipe(surv ~ x1 + x2 + subject_id, data = d), "subject_id", new_role = "predictor")
  expect_error(cv_coxnet(bad, d, subject_id = "subject_id", v = 3, eval_time = c(3, 6)), "is a predictor")
})

test_that("cv_coxnet handles right-censored x/y data and validates grouping", {
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  last <- d[!duplicated(d$subject_id, fromLast = TRUE), ]
  set.seed(4)
  cv <- cv_coxnet(
    last[c("x1", "x2", "x3")], survival::Surv(last$tstop, last$status), v = 3,
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    eval_time = c(3, 6, 9), nlambda = 6
  )
  expect_s3_class(cv, "cv_coxnet")

  y <- survival::Surv(d$tstart, d$tstop, d$status)
  expect_error(cv_coxnet(d[c("x1", "x2", "x3")], y, v = 3, eval_time = c(3, 6)), "subject_id")

  wandering <- d
  wandering$site[1] <- "elsewhere"
  wandering$site[2] <- "site_1"
  wandering$subject_id[1:2] <- 1
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = wandering,
      subject_id = "subject_id", group = "site", v = 3, eval_time = c(3, 6)
    ),
    "single `group`"
  )
  expect_error(
    cv_coxnet(survival::Surv(tstart, tstop, status) ~ x1 + x2, data = d, subject_id = "nope"),
    "name of a column"
  )
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ x1 + x2, data = d, subject_id = "subject_id",
      metrics = yardstick::metric_set(yardstick::rmse)
    ),
    "survival metrics"
  )
})

test_that("cv_coxnet scores bootstrap resamples, which repeat subjects", {
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  set.seed(8)
  boots <- rsample::group_bootstraps(d, group = subject_id, times = 3)
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d,
    subject_id = "subject_id", resamples = boots, eval_time = c(3, 6, 9),
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    nlambda = 5, cox.ties = "breslow"
  )
  expect_setequal(unique(cv$fold_metrics$id), boots$id)
  expect_true(all(!is.na(cv$fold_metrics$.estimate)))
})

test_that("collect_metrics works on cv_coxnet results", {
  skip_if_no_coxnet_deps()
  skip_if_not_installed("tune")
  d <- sim_counting()
  last <- d[!duplicated(d$subject_id, fromLast = TRUE), ]
  set.seed(5)
  cv <- cv_coxnet(
    last[c("x1", "x2", "x3")], survival::Surv(last$tstop, last$status), v = 3,
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    eval_time = c(3, 6, 9), nlambda = 5
  )
  expect_identical(tune::collect_metrics(cv), cv$metrics)
  expect_identical(tune::collect_metrics(cv, summarize = FALSE), cv$fold_metrics)
})
