tuned_workflow_set <- function() {
  for (pkg in c("workflowsets", "tune", "parsnip", "workflows", "rsample", "yardstick", "glmnet")) {
    skip_if_not_installed(pkg)
  }
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 3)
  lasso <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(), mixture = 1), "glmnet")
  wset <- workflowsets::workflow_set(
    preproc = list(all = mpg ~ .),
    models = list(lm = parsnip::linear_reg(), lasso = lasso)
  )
  suppressWarnings(suppressMessages(workflowsets::workflow_map(
    wset, "tune_grid",
    resamples = folds, grid = 3, seed = 1,
    metrics = yardstick::metric_set(yardstick::rmse),
    control = tune::control_grid(save_workflow = TRUE)
  )))
}

test_that("get_model_parameters returns the ranked workflow's best parameters", {
  results <- tuned_workflow_set()
  ranked <- workflowsets::rank_results(results, rank_metric = "rmse", select_best = TRUE)

  best <- get_model_parameters(results, "rmse")
  expect_equal(nrow(best), 1)
  expect_equal(best$wflow_id, ranked$wflow_id[ranked$rank == 1][1])
  expect_true(all(c("wflow_id", "preproc", "model") %in% names(best)))

  second <- get_model_parameters(results, "rmse", .rank = 2)
  expect_false(identical(second$wflow_id, best$wflow_id))

  expect_error(get_model_parameters(results, "rmse", .rank = 3), "No workflow at rank 3")
})

test_that("fit_n_rank fits the best workflow and reports its ranking row", {
  results <- tuned_workflow_set()
  best <- fit_n_rank(results, rank_metric = "rmse")

  expect_s3_class(best$model, "workflow")
  expect_equal(nrow(best$tuned_parameters), 1)
  expect_equal(best$tuned_parameters$rank, 1)
  expect_true(all(c("mean", "std_err", ".config", "overall_rank") %in% names(best$tuned_parameters)))
  expect_equal(
    best$tuned_parameters$wflow_id,
    get_model_parameters(results, "rmse")$wflow_id
  )
})

test_that("fit_n_rank with group_wflow = FALSE fits the ranked configuration, not the workflow's best", {
  results <- tuned_workflow_set()
  rankings <- workflowsets::rank_results(results, rank_metric = "rmse", select_best = FALSE)
  rankings <- rankings[rankings$.metric == "rmse", ]
  # Pick a lasso configuration that is not the lasso workflow's best.
  lasso <- rankings[rankings$wflow_id == "all_lasso", ]
  target <- lasso[nrow(lasso), ]

  fitted <- fit_n_rank(results, rank_metric = "rmse", .rank = target$rank, group_wflow = FALSE)
  expect_equal(fitted$tuned_parameters$.config, target$.config)

  tune_res <- workflowsets::extract_workflow_set_result(results, "all_lasso")
  metrics <- tune::collect_metrics(tune_res)
  expect_equal(fitted$tuned_parameters$penalty, metrics$penalty[metrics$.config == target$.config][1])
})
