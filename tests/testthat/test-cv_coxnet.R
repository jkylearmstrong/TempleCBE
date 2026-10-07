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

test_that("cv_coxnet refuses supplied resamples that put a subject in both sets of a split", {
  # A row-level vfold_cv() on start/stop data ran with no error and scored each
  # model on subjects it was fit on (28, 26 and 33 of 80 subjects per fold).
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  run <- function(resamples, ..., data = d, formula = f, subject_id = "subject_id") {
    cv_coxnet(
      formula, data = data, subject_id = subject_id, resamples = resamples,
      eval_time = c(3, 6, 9), metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
      nlambda = 5, cox.ties = "breslow", ...
    )
  }

  set.seed(10)
  by_row <- rsample::vfold_cv(d, v = 3)
  shared <- vapply(by_row$splits, function(s) {
    length(intersect(rsample::analysis(s)$subject_id, rsample::assessment(s)$subject_id))
  }, integer(1))
  expect_true(all(shared > 0))
  expect_error(run(by_row), "subjects in both the analysis and the assessment set of resample `\\w+`")
  expect_error(run(by_row), "check_subject_overlap = FALSE")

  # Folds grouped by subject, and bootstrap out-of-bag sets, are disjoint by construction.
  by_subject <- rsample::group_vfold_cv(d, group = subject_id, v = 3)
  expect_s3_class(run(by_subject), "cv_coxnet")
  boots <- rsample::group_bootstraps(d, group = subject_id, times = 2)
  expect_s3_class(run(boots), "cv_coxnet")

  # The opt-out lets people who overlap deliberately carry on.
  leaky <- run(by_row, check_subject_overlap = FALSE)
  expect_s3_class(leaky, "cv_coxnet")
  expect_error(run(by_row, check_subject_overlap = NA), "TRUE or FALSE")

  # With `group`, the unit that must stay on one side is the group.
  by_site <- rsample::group_vfold_cv(d, group = site, v = 4)
  expect_error(run(by_subject, group = "site"), "groups in both the analysis and the assessment set")
  expect_s3_class(run(by_site, group = "site"), "cv_coxnet")

  # Right-censored data with one row per subject: row-level folds are fine.
  last <- d[!duplicated(d$subject_id, fromLast = TRUE), ]
  rc <- run(rsample::vfold_cv(last, v = 3), data = last, formula = survival::Surv(tstop, status) ~ x1 + x2 + x3, subject_id = NULL)
  expect_s3_class(rc, "cv_coxnet")

  # Folds that cv_coxnet builds itself are never flagged.
  own <- cv_coxnet(
    f, data = d, subject_id = "subject_id", v = 3, eval_time = c(3, 6, 9),
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated), nlambda = 5
  )
  expect_s3_class(own, "cv_coxnet")
})

test_that("complete_assessment_rows drops a subject with any missing predictor, with one warning", {
  x <- cbind(a = c(1, 2, NA, 4, 5, 6), b = c(1, 1, 1, 1, NA, 1))
  subject <- c("s1", "s1", "s2", "s2", "s3", "s4")
  got <- collect_warnings(complete_assessment_rows(x, subject, "resample `Fold1`"))
  # s2 has one incomplete row and s3 its only row: both subjects go, with all their rows.
  expect_equal(got$value, c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_length(got$warnings, 1)
  expect_match(got$warnings, "2 of 4 subject\\(s\\) in resample `Fold1` have missing predictor values")
  expect_no_warning(complete_assessment_rows(x[c(1, 2, 6), ], subject[c(1, 2, 6)], "resample `Fold1`"))
  expect_equal(suppressWarnings(complete_assessment_rows(x[c(3, 5), ], subject[c(3, 5)], "here")), c(FALSE, FALSE))
})

test_that("cv_coxnet scores every penalty on the same subjects when a predictor level is unseen in a fold", {
  # A factor level the analysis set never saw becomes NA in the assessment set
  # (hardhat warns only). Its subject was scored at penalties where the level's
  # coefficient is 0 and dropped by yardstick at the others, so the folds'
  # means were over different subjects; an all-unseen fold crashed in yardstick.
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 60)
  d$grade <- ifelse(d$subject_id %% 2 == 0, "even", "odd")
  d$grade[d$subject_id == 7] <- "rare"  # one subject: unseen in the analysis set of the fold that holds it out
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + grade
  metrics <- yardstick::metric_set(yardstick::brier_survival_integrated)

  set.seed(14)
  folds <- rsample::group_vfold_cv(d, group = subject_id, v = 4)
  unseen <- vapply(folds$splits, function(s) {
    "rare" %in% rsample::assessment(s)$grade && !"rare" %in% rsample::analysis(s)$grade
  }, logical(1))
  expect_equal(sum(unseen), 1)

  got <- collect_warnings(cv_coxnet(
    f, data = d, subject_id = "subject_id", resamples = folds, metrics = metrics,
    eval_time = c(3, 6, 9), nlambda = 6, cox.ties = "breslow"
  ))
  ours <- grep("left out of the scores at every penalty", got$warnings, value = TRUE)
  expect_length(ours, 1)
  expect_match(ours, paste0("1 of \\d+ subject\\(s\\) in resample `", folds$id[unseen], "`"))
  expect_s3_class(got$value, "cv_coxnet")
  expect_false(anyNA(got$value$fold_metrics$.estimate))
  expect_setequal(unique(got$value$fold_metrics$id), folds$id)

  # Every assessment subject unseen (site as a predictor, leave-one-site-out):
  # nothing can be scored, which is now said plainly instead of crashing in yardstick.
  sites <- rsample::group_vfold_cv(d, group = site, v = 4)
  all_novel <- collect_warnings(try(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + site, data = d, subject_id = "subject_id",
      resamples = sites, eval_time = c(3, 6, 9), nlambda = 5,
      metrics = yardstick::metric_set(yardstick::concordance_survival)
    ),
    silent = TRUE
  ))
  expect_s3_class(all_novel$value, "try-error")
  expect_match(as.character(all_novel$value), "No resample could be fit")
  expect_true(any(grepl("which leaves none to score", all_novel$warnings)))
})

test_that("strata() and offset() in a formula are refused by cv_coxnet and nested_cv_coxnet", {
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 40)
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ x1 + x2 + offset(x3), data = d,
      subject_id = "subject_id", v = 3, eval_time = c(3, 6), nlambda = 5
    ),
    "strata\\(\\) and offset\\(\\) are not supported"
  )
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ x1 + x2 + survival::strata(site), data = d,
      subject_id = "subject_id", v = 3, eval_time = c(3, 6), nlambda = 5
    ),
    "strata\\(\\) and offset\\(\\) are not supported"
  )
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(
    nested_cv_coxnet(folds, survival::Surv(tstart, tstop, status) ~ x1 + strata(site), subject_id = "subject_id"),
    "strata\\(\\) and offset\\(\\) are not supported"
  )
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
