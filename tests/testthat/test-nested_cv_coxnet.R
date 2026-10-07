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

test_that("nested_cv_coxnet refuses outer splits and inner resamples that share subjects", {
  # Outer row-level folds put the same subjects in the outer analysis and
  # assessment sets, so the 'unseen' outer metrics were optimistic.
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 60)
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  metrics <- yardstick::metric_set(yardstick::brier_survival_integrated)
  run <- function(folds, ...) {
    nested_cv_coxnet(
      folds, f, subject_id = "subject_id", metrics = metrics, eval_time = c(3, 6, 9),
      nlambda = 5, cox.ties = "breslow", ...
    )
  }

  set.seed(12)
  leaky_outer <- rsample::nested_cv(
    d,
    outside = rsample::vfold_cv(v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(run(leaky_outer), "outer splits of `object` put .* subjects in both the analysis and the assessment set")

  leaky_inner <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::vfold_cv(v = 2)
  )
  expect_error(
    run(leaky_inner),
    "inner resamples of outer split `\\w+` put .* subjects in both the analysis and the assessment set of resample `\\w+`"
  )

  # The opt-out lets deliberate overlap through; grouped folds and bootstraps pass as they are.
  expect_s3_class(run(leaky_inner, check_subject_overlap = FALSE), "nested_cv_coxnet")
  expect_error(run(leaky_inner, check_subject_overlap = "no"), "TRUE or FALSE")
  boot_inner <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_bootstraps(group = subject_id, times = 2)
  )
  expect_s3_class(run(boot_inner), "nested_cv_coxnet")
})

test_that("nested_cv_coxnet leaves out outer assessment subjects whose predictors are missing", {
  # The refit on the outer analysis set has never seen the "rare" level, so the
  # one subject that carries it cannot be predicted in the outer assessment set.
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 60)
  d$grade <- ifelse(d$subject_id %% 2 == 0, "even", "odd")  # both levels in every outer analysis set
  d$grade[d$subject_id == 7] <- "rare"
  set.seed(15)
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  got <- collect_warnings(nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + grade, subject_id = "subject_id",
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    eval_time = c(3, 6, 9), nlambda = 5, cox.ties = "breslow"
  ))
  expect_s3_class(got$value, "nested_cv_coxnet")
  expect_true(any(grepl("1 of \\d+ subject\\(s\\) in the outer assessment set of split `\\w+`", got$warnings)))
  expect_false(anyNA(unlist(lapply(got$value$.metrics, `[[`, ".estimate"))))

  # Nothing left to score in an outer split is an error that names the split.
  sites <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = site, v = 4),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(
    collect_warnings(nested_cv_coxnet(
      sites, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + site, subject_id = "subject_id",
      metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
      eval_time = c(3, 6, 9), nlambda = 5
    )),
    "Outer split `\\w+` has no assessment subject to score"
  )
})

test_that("nested_cv_coxnet(importance = 'loco_mp') fits the minipatches with the glmnet arguments of the tuned model", {
  # The audit's trace showed `names(list(...))` empty in every cbe_loco_mp_coxnet()
  # call, so importance came from glmnet defaults, not from the assessed model.
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 60)
  set.seed(16)
  folds <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  seen <- list()
  # coxnet() is only called by the minipatches; the tuning and the refit use glmnet directly.
  testthat::local_mocked_bindings(coxnet = function(x, y, ...) {
    seen[[length(seen) + 1]] <<- list(...)
    stop("mocked coxnet", call. = FALSE)
  })
  got <- collect_warnings(nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, subject_id = "subject_id",
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated), eval_time = c(3, 6, 9),
    importance = "loco_mp", nlambda = 5, standardize = FALSE, cox.ties = "efron", B = 7
  ))
  # B = 30 patches on each of the 2 outer splits, and the `B = 7` in `...` was not forwarded.
  expect_length(seen, 60)
  expect_true(all(vapply(seen, function(a) identical(a$standardize, FALSE), logical(1))))
  expect_true(all(vapply(seen, function(a) identical(a$cox.ties, "efron"), logical(1))))
  expect_true(all(vapply(seen, function(a) identical(a$nlambda, 5), logical(1))))
  expect_false(any(grepl("matched by multiple", got$warnings)))
  expect_length(grep("LOCO-MP importance failed for outer split .*first error was: mocked coxnet", got$warnings), 2)
})

test_that("forwardable_glmnet_args keeps the named glmnet arguments and drops what cbe_loco_mp_coxnet sets", {
  args <- list(standardize = FALSE, B = 7, cox.ties = "efron", 5, penalty = 0.1, seed = 1, nlambda = 4)
  expect_identical(
    forwardable_glmnet_args(args),
    list(standardize = FALSE, cox.ties = "efron", nlambda = 4)
  )
  expect_identical(forwardable_glmnet_args(list()), list())
  expect_identical(forwardable_glmnet_args(list(1, 2)), list())
})

test_that("nested_cv_coxnet warns that `group` does not apply to the LOCO-MP importance", {
  skip_if_no_coxnet_deps()
  d <- sim_counting(n = 60)
  set.seed(17)
  by_site <- rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = site, v = 2),
    inside = rsample::group_vfold_cv(group = site, v = 2)
  )
  testthat::local_mocked_bindings(coxnet = function(x, y, ...) stop("mocked coxnet", call. = FALSE))
  run <- function(...) {
    collect_warnings(nested_cv_coxnet(
      by_site, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, subject_id = "subject_id",
      metrics = yardstick::metric_set(yardstick::brier_survival_integrated), eval_time = c(3, 6, 9),
      nlambda = 5, ...
    ))$warnings
  }
  expect_true(any(grepl("`group` does not apply to the LOCO-MP importance", run(group = "site", importance = "loco_mp"))))
  expect_false(any(grepl("does not apply", run(group = "site", importance = "none"))))
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
