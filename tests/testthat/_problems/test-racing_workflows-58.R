# Extracted from test-racing_workflows.R:58

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TempleCBE", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_if_no_racing_deps <- function() {
  for (pkg in c("finetune", "parsnip", "workflows", "rsample", "yardstick", "survival", "glmnet")) {
    testthat::skip_if_not_installed(pkg)
  }
}

# test -------------------------------------------------------------------------
skip_if_no_racing_deps()
vet <- stats::na.omit(survival::veteran[1:80, ])
set.seed(1503)
folds <- rsample::vfold_cv(vet, v = 3)
spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")
wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(survival::Surv(time, status) ~ karno + diagtime + age)
ctrl <- control_race_survival(burn_in = 2, randomize = FALSE)
res <- tune_race_survival(
    wflow,
    resamples = folds,
    fn = "tune_race_anova",
    grid = 5,
    eval_time = c(30, 90),
    control = ctrl,
    seed = 1503
  )
expect_s3_class(res, "tune_results")
metrics <- tune::collect_metrics(res)
expect_true(nrow(metrics) > 0)
expect_true(all(c("penalty", ".metric", "mean") %in% names(metrics)))
expect_true(is.function(cbe_tune_race_survival))
