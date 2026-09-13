# Synthetic start/stop data: each subject has 1-4 intervals, with an event
# (driven by x1) possible only on the last one.
sim_start_stop <- function(n = 60, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:4, 1)
    stops <- c(5, 10, 20, 40)[seq_len(k)]
    x1 <- stats::rnorm(1)
    data.frame(
      id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(2 * x1))),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

ibs_fixture <- function() {
  long <- sim_start_stop()
  rec <- recipes::recipe(~ ., data = long)
  rec <- recipes::update_role(rec, "id", "tstart", "tstop", "status", new_role = "id variable")
  rec <- recipes::step_range(rec, recipes::all_numeric_predictors())
  set.seed(2)
  list(
    split = rsample::group_initial_split(long, group = id, prop = 0.7),
    recipe = rec,
    features = c("x1", "x2", "x3"),
    times = unique(long[, c("tstart", "tstop")]),
    long = long
  )
}

skip_if_no_ibs_deps <- function() {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) skip_if_not_installed(pkg)
}

test_that("glmnet_IBS returns one row per feature with a valid IBS under both covariate settings", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()

  for (covariates in c("path", "baseline")) {
    set.seed(3)
    res <- glmnet_IBS(
      fx$split, alpha = 0.5, recipe = fx$recipe, feature_names = fx$features,
      time_data = fx$times, internal_folds = 3, cox.ties = "breslow", covariates = covariates
    )
    expect_s3_class(res, "tbl_df")
    expect_named(res, c("IBS", "lambda", "term", "estimate", "alpha"))
    expect_equal(res$term, fx$features)
    expect_length(unique(res$IBS), 1)
    expect_true(res$IBS[1] >= 0 && res$IBS[1] <= 1)
    expect_equal(unique(res$alpha), 0.5)
  }
})

test_that("glmnet_IBS chooses lambda with cv_coxnet on the baked analysis set", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  set.seed(3)
  res <- glmnet_IBS(
    fx$split, alpha = 1, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", nlambda = 10
  )

  train <- recipes::bake(
    recipes::prep(fx$recipe, training = rsample::analysis(fx$split)[rsample::analysis(fx$split)$tstart == 0, ]),
    new_data = rsample::analysis(fx$split)
  )
  set.seed(3)
  cv <- cv_coxnet(
    as.data.frame(train[fx$features]), survival::Surv(train$tstart, train$tstop, train$status),
    subject_id = train$id, v = 3, eval_time = c(5, 10, 20, 40),
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
    nlambda = 10, cox.ties = "breslow"
  )
  expect_equal(res$lambda[1], cv$lambda_min)
  expect_equal(res$estimate, generics::tidy(cv)$estimate)

  set.seed(3)
  one_se <- glmnet_IBS(
    fx$split, alpha = 1, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", nlambda = 10, rule = "1se"
  )
  expect_gte(one_se$lambda[1], res$lambda[1])
})

test_that("glmnet_IBS restricts the model to `formula` terms", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  set.seed(3)
  res <- glmnet_IBS(
    fx$split, alpha = 0, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", formula = "x1 + x2"
  )
  expect_setequal(res$term, c("x1", "x2"))
})

test_that("glmnet_IBS reports failure_ibs, with a warning, when the model cannot be fit", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  expect_warning(
    res <- glmnet_IBS(
      fx$split, recipe = fx$recipe, feature_names = fx$features,
      time_data = fx$times, internal_folds = 1
    ),
    "could not fit"
  )
  expect_equal(res, tibble::tibble(IBS = NA_real_, lambda = NA_real_, alpha = 1))

  expect_warning(
    res2 <- glmnet_IBS(
      fx$split, recipe = fx$recipe, feature_names = fx$features,
      time_data = fx$times, internal_folds = 1, failure_ibs = 2
    ),
    "could not fit"
  )
  expect_equal(res2$IBS, 2)
})

test_that("glmnet_IBS validates its inputs and retired options", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  expect_error(
    glmnet_IBS(fx$long, recipe = fx$recipe, feature_names = fx$features, time_data = fx$times),
    "must be an rsplit"
  )
  expect_error(
    glmnet_IBS(fx$split, recipe = fx$recipe, feature_names = fx$features, time_data = fx$times["tstop"]),
    "missing column\\(s\\): tstart"
  )
  expect_error(
    glmnet_IBS(fx$split, recipe = fx$recipe, feature_names = fx$features, censoring_weights = "none"),
    "0\\.2\\.0"
  )
  expect_error(
    glmnet_IBS(fx$split, recipe = fx$recipe, feature_names = fx$features, metric = "not_a_metric"),
    "yardstick survival metric"
  )
  expect_error(
    glmnet_IBS(fx$split, recipe = fx$recipe, feature_names = fx$features, type.measure = "deviance"),
    "replaced by `metric`"
  )
})

test_that("glmnet_IBS accepts feature_names as a function of the baked data", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  set.seed(3)
  res <- glmnet_IBS(
    fx$split, alpha = 0, recipe = fx$recipe,
    feature_names = function(baked) grep("^x[12]$", names(baked), value = TRUE),
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow"
  )
  expect_setequal(res$term, c("x1", "x2"))
})

test_that("alpha_grid has num_fixed even steps plus one random value per gap", {
  set.seed(10)
  grid <- alpha_grid(10, 6)
  fixed <- seq(0, 1, length.out = 6)
  expect_length(grid, 10)
  expect_false(is.unsorted(grid))
  expect_true(all(fixed %in% grid))

  randoms <- setdiff(grid, fixed)
  gaps <- findInterval(randoms, fixed)
  expect_setequal(gaps, 1:4)

  set.seed(10)
  expect_identical(alpha_grid(10, 6), grid)

  expect_length(alpha_grid(3, 3), 3)
  expect_error(alpha_grid(10, 4.2), "whole number")
  expect_error(alpha_grid(5, 6), "no smaller than")
})

test_that("tune_over_alpha returns safely() results named by alpha", {
  skip_if_no_ibs_deps()
  skip_if_not_installed("furrr")
  fx <- ibs_fixture()
  runs <- tune_over_alpha(
    fx$split, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", alphas = c(1, 0.5)
  )
  expect_named(runs, c("0.5", "1"))
  expect_true(all(vapply(runs, function(run) is.null(run$error), logical(1))))
  expect_equal(unique(runs[["1"]]$result$alpha), 1)
})

test_that("summarize_tune_results binds successful fits across splits", {
  skip_if_no_ibs_deps()
  skip_if_not_installed("furrr")
  fx <- ibs_fixture()
  set.seed(4)
  folds <- rsample::group_vfold_cv(rsample::training(fx$split), group = id, v = 2)
  res <- summarize_tune_results(
    folds, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", alphas = c(0.5, 1)
  )
  expect_s3_class(res, "tbl_df")
  expect_setequal(unique(res$inner_resamples_splits), c("1", "2"))
  expect_setequal(unique(res$alpha), c(0.5, 1))
})

test_that("tune_over_alpha fits one model per formula and records it", {
  skip_if_no_ibs_deps()
  skip_if_not_installed("furrr")
  fx <- ibs_fixture()
  runs <- tune_over_alpha(
    fx$split, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow",
    formulas = c("x1 + x2", "x2 + x3"), alphas = c(0, 0.5)
  )
  expect_named(runs, c("0", "0.5"))
  results <- lapply(runs, `[[`, "result")
  expect_equal(unique(results[[1]]$formula), "x1 + x2")
  expect_setequal(results[[2]]$term, c("x2", "x3"))

  set.seed(8)
  drawn <- tune_over_alpha(
    fx$split, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow",
    formulas = "x1 + x3"
  )
  expect_length(drawn, 1)
  expect_true(all(unique(drawn[[1]]$result$alpha) >= 0 & unique(drawn[[1]]$result$alpha) <= 1))

  expect_error(tune_over_alpha(fx$split, formulas = "x1 + x2", alphas = c(0, 1)), "one value per")
  expect_error(tune_over_alpha(fx$split, formulas = "x1 + x2", formula = "x1"), "not both")
})

test_that("tune_over_alpha accepts formula objects in formulas", {
  skip_if_no_ibs_deps()
  skip_if_not_installed("furrr")
  fx <- ibs_fixture()
  runs <- tune_over_alpha(
    fx$split, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow",
    formulas = list(~ x1 + x2, ~ x2 + x3), alphas = c(0, 0.5)
  )
  results <- lapply(runs, `[[`, "result")
  expect_equal(unique(results[[1]]$formula), "x1 + x2")
  expect_setequal(results[[1]]$term, c("x1", "x2"))
  expect_setequal(results[[2]]$term, c("x2", "x3"))
})
