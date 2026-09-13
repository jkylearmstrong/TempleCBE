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
  for (pkg in c("glmnet", "survival", "rsample", "yardstick")) skip_if_not_installed(pkg)
}

test_that("glmnet_IBS returns one row per coefficient at lambda.min under both weightings", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()

  for (weights in c("none", "ipcw")) {
    set.seed(3)
    res <- glmnet_IBS(
      fx$split, alpha = 0.5, recipe = fx$recipe, feature_names = fx$features,
      time_data = fx$times, internal_folds = 3, cox.ties = "breslow", censoring_weights = weights
    )
    expect_s3_class(res, "tbl_df")
    expect_named(res, c("IBS", "lambda", "term", "estimate", "alpha"))
    expect_true(all(res$term %in% fx$features))
    expect_length(unique(res$IBS), 1)
    expect_true(is.finite(res$IBS[1]))
    expect_equal(unique(res$alpha), 0.5)
  }

  set.seed(3)
  ipcw <- glmnet_IBS(
    fx$split, alpha = 0.5, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3, cox.ties = "breslow", censoring_weights = "ipcw"
  )
  expect_true(ipcw$IBS[1] >= 0 && ipcw$IBS[1] <= 1)
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

test_that("glmnet_IBS reports failure_ibs when cv.glmnet cannot fit", {
  skip_if_no_ibs_deps()
  fx <- ibs_fixture()
  # cv.glmnet() refuses fewer than 3 folds.
  res <- glmnet_IBS(
    fx$split, recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 1
  )
  expect_equal(res, tibble::tibble(IBS = 2, lambda = 0, alpha = 1))
})

test_that("glmnet_IBS validates its inputs", {
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

test_that("fill_relative_risk fills from the last known value and refuses subjects with none", {
  filled <- fill_relative_risk(data.frame(id = 1, relative_risk = c(1, 3, NA, NA)), "id")
  expect_equal(filled$relative_risk, c(1, 3, 3, 3))

  expect_error(
    fill_relative_risk(data.frame(id = c(1, 1, 2, 2), relative_risk = c(1, 2, NA, NA)), "id"),
    "no known value: 2"
  )
})

test_that("censoring_prob steps at censoring times, with a left limit", {
  skip_if_not_installed("survival")
  # Censored at 2 and 6 (status 0 there); events at 4 and 8.
  cens_fit <- survival::survfit(survival::Surv(c(2, 4, 6, 8), 1 - c(0, 1, 0, 1)) ~ 1)
  expect_equal(censoring_prob(c(1, 2, 3, 6), cens_fit), c(1, 0.75, 0.75, 0.375))
  expect_equal(censoring_prob(c(2, 6), cens_fit, left = TRUE), c(1, 0.75))
})

test_that("ibs_rows_ipcw applies Graf weights, matching a hand-computed Brier score", {
  skip_if_not_installed("survival")
  skip_if_not_installed("yardstick")

  # Training subjects give G(t) = 1 before 2, 3/4 on [2, 6), 3/8 from 6.
  train_raw <- data.frame(id = 1:4, tstop = c(2, 4, 6, 8), status = c(0, 1, 0, 1))
  # Subject 11 has an event at 5; subject 12 is censored at 7 (two intervals).
  test_raw <- data.frame(id = c(11, 12, 12), tstop = c(5, 3, 7), status = c(1, 0, 0))
  test <- data.frame(
    id = rep(c(11, 12), each = 2),
    .eval_time = rep(c(3, 6), 2),
    .pred_survival = c(0.9, 0.5, 0.8, 0.6)
  )

  rows <- ibs_rows_ipcw(test, train_raw, test_raw, c(3, 6), "id", "tstop", "status")
  expect_equal(rows$.time, c(5, 7))
  expect_equal(rows$.status, c(1, 0))

  weights <- lapply(rows$.pred, `[[`, ".weight_censored")
  # 11: at risk at 3 -> 1/G(3); event by 6 -> 1/G(5-). 12: at risk at both -> 1/G(3), 1/G(6).
  expect_equal(weights[[1]], c(4 / 3, 4 / 3))
  expect_equal(weights[[2]], c(4 / 3, 8 / 3))

  bs <- yardstick::brier_survival(rows, truth = .truth, .pred)
  # t = 6: 11 had the event -> 0.5^2 * 4/3; 12 still at risk -> 0.4^2 * 8/3; over n = 2.
  expect_equal(bs$.estimate[bs$.eval_time == 6], (0.25 * 4 / 3 + 0.16 * 8 / 3) / 2)
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
