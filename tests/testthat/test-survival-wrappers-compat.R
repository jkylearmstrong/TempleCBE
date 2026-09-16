# Downstream analysis code calls glmnet_IBS(), tune_over_alpha(), and
# summarize_tune_results() with the arguments exercised here. These tests pin
# that interface: add arguments freely, but don't rename or remove these.

compat_data <- function(n = 60, seed = 11) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:3, 1)
    stops <- c(5, 10, 20)[seq_len(k)]
    x1 <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * x1))),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

compat_fixture <- function() {
  long <- compat_data()
  rec <- recipes::recipe(~ ., data = long)
  rec <- recipes::update_role(rec, "subject_id", "tstart", "tstop", "status", new_role = "id variable")
  rec <- recipes::step_range(rec, recipes::all_numeric_predictors())
  set.seed(12)
  list(
    long = long,
    split = rsample::group_initial_split(long, group = subject_id, prop = 0.7),
    recipe = rec,
    features = c("x1", "x2", "x3"),
    times = dplyr::arrange(dplyr::distinct(long, tstart, tstop), tstart, tstop)
  )
}

skip_if_no_compat_deps <- function() {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) skip_if_not_installed(pkg)
}

test_that("glmnet_IBS keeps the arguments downstream call sites use", {
  args <- names(formals(glmnet_IBS))
  expect_true(all(c(
    "object", "alpha", "recipe", "feature_names", "time_data", "formula",
    "internal_folds", "id_col", "censoring_weights", "..."
  ) %in% args))
  expect_identical(args[1:2], c("object", "alpha"))
})

test_that("tune_over_alpha and summarize_tune_results keep their arguments", {
  for (fn in list(tune_over_alpha, summarize_tune_results)) {
    args <- names(formals(fn))
    expect_identical(args[1:2], c("object", "..."))
    expect_true(all(c("num_alpha_values", "num_fixed", "alphas", "formulas") %in% args))
  }
})

test_that("a downstream-style glmnet_IBS call runs and returns IBS, lambda, term, estimate, alpha", {
  skip_if_no_compat_deps()
  fx <- compat_fixture()
  set.seed(13)
  res <- glmnet_IBS(
    fx$split,
    alpha = 0.5,
    recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3,
    id_col = "subject_id", censoring_weights = "ipcw", cox.ties = "breslow"
  )
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("IBS", "lambda", "term", "estimate", "alpha") %in% names(res)))
  expect_equal(unique(res$alpha), 0.5)
  expect_length(unique(res$IBS), 1)
  expect_true(is.na(res$IBS[1]) || (res$IBS[1] >= 0 && res$IBS[1] <= 1))
})

test_that("downstream-style tune_over_alpha and summarize_tune_results calls run", {
  skip_if_no_compat_deps()
  skip_if_not_installed("furrr")
  fx <- compat_fixture()

  set.seed(14)
  runs <- tune_over_alpha(
    fx$split,
    recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3,
    id_col = "subject_id", censoring_weights = "ipcw", cox.ties = "breslow",
    num_alpha_values = 3, num_fixed = 2
  )
  expect_length(runs, 3)
  expect_true(all(vapply(runs, function(run) is.null(run$error), logical(1))))

  set.seed(15)
  nested <- rsample::nested_cv(
    fx$long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_bootstraps(group = subject_id, times = 2)
  )
  res <- summarize_tune_results(
    nested$inner_resamples[[1]],
    recipe = fx$recipe, feature_names = fx$features,
    time_data = fx$times, internal_folds = 3,
    id_col = "subject_id", censoring_weights = "ipcw", cox.ties = "breslow",
    num_alpha_values = 2, num_fixed = 2
  )
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("inner_resamples_splits", "IBS", "lambda", "term", "estimate", "alpha") %in% names(res)))
  expect_setequal(unique(res$inner_resamples_splits), c("1", "2"))
})
