# Unit and Integration Tests for Racing Workflows (tests/testthat/test-racing_workflows.R)

skip_if_no_racing_deps <- function() {
  for (pkg in c("finetune", "parsnip", "workflows", "rsample", "yardstick", "survival", "glmnet")) {
    testthat::skip_if_not_installed(pkg)
  }
}

test_that("control_race_survival produces valid control specification with clinical defaults", {
  skip_if_no_racing_deps()

  ctrl <- control_race_survival()
  expect_s3_class(ctrl, "control_race")
  expect_true(ctrl$save_pred)
  expect_true(ctrl$save_workflow)
  expect_equal(ctrl$parallel_over, "everything")
  expect_equal(ctrl$burn_in, 3)

  # Check alias
  ctrl_cbe <- cbe_control_race(burn_in = 4)
  expect_equal(ctrl_cbe$burn_in, 4)
})

test_that("tune_race_survival runs ANOVA racing on a survival workflow", {
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

  # Check cbe alias
  expect_identical(cbe_tune_race_survival, tune_race_survival)
})

test_that("tune_race_survival runs win-loss racing on a survival workflow", {
  skip_if_no_racing_deps()
  skip_if_not_installed("BradleyTerry2")

  vet <- stats::na.omit(survival::veteran[1:80, ])
  set.seed(1503)
  folds <- rsample::vfold_cv(vet, v = 3)

  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 0.5),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")

  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(survival::Surv(time, status) ~ karno + age)

  ctrl <- control_race_survival(burn_in = 2, num_ties = 5, randomize = FALSE)

  res <- tune_race_survival(
    wflow,
    resamples = folds,
    fn = "tune_race_win_loss",
    grid = 4,
    eval_time = c(30, 90),
    control = ctrl,
    seed = 1503
  )

  expect_s3_class(res, "tune_results")
  best <- tune::select_best(res, metric = "brier_survival_integrated")
  expect_true(is.numeric(best$penalty) && best$penalty > 0)
})

test_that("tune_race_survival seamlessly maps across workflow_set via workflow_map", {
  skip_if_no_racing_deps()
  skip_if_not_installed("workflowsets")

  vet <- stats::na.omit(survival::veteran[1:80, ])
  set.seed(1503)
  folds <- rsample::vfold_cv(vet, v = 3)

  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")

  # Preprocessor formulas
  wf_set <- workflowsets::workflow_set(
    preproc = list(
      full = survival::Surv(time, status) ~ karno + diagtime + age,
      sub  = survival::Surv(time, status) ~ karno + age
    ),
    models = list(coxnet = spec)
  )

  ctrl <- control_race_survival(burn_in = 2, randomize = FALSE)

  # Map racing across all workflows
  mapped <- tune_race_survival(
    wf_set,
    resamples = folds,
    fn = "tune_race_anova",
    grid = 4,
    eval_time = c(30, 90),
    control = ctrl,
    seed = 1503
  )

  expect_s3_class(mapped, "workflow_set")
  expect_equal(nrow(mapped), 2)
  expect_true(all(vapply(mapped$result, function(r) inherits(r, "tune_results"), logical(1))))
})

test_that("nested_cv_coxnet integrates racing tuning on inner resamples", {
  skip_if_no_racing_deps()

  vet <- stats::na.omit(survival::veteran[1:90, ])
  nested <- rsample::nested_cv(
    vet,
    outside = rsample::vfold_cv(v = 2),
    inside = rsample::vfold_cv(v = 3)
  )

  res_race <- nested_cv_coxnet(
    nested,
    preprocessor = survival::Surv(time, status) ~ karno + diagtime + age,
    tune_method = "race_anova",
    check_subject_overlap = FALSE
  )

  expect_s3_class(res_race, "nested_cv_coxnet")
  expect_equal(nrow(res_race), 2)
  expect_true(all(is.finite(res_race$penalty)))
  expect_true(all(res_race$penalty > 0))
  expect_true(!is.null(res_race$.metrics[[1]]))
})


# ---- Defaults and tidymodels interoperability ---------------------------------------------------

racing_workflow <- function(formula = survival::Surv(time, status) ~ karno + diagtime + age) {
  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")
  workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(formula)
}

test_that("tune_race_survival picks the event-time deciles when eval_time is not given", {
  skip_if_no_racing_deps()

  vet <- stats::na.omit(survival::veteran[1:90, ])
  set.seed(1503)
  folds <- rsample::vfold_cv(vet, v = 3)

  # tune needs >= 2 evaluation times for the integrated metrics and has no default, so racing a
  # survival workflow without them used to stop in check_enough_eval_times()
  res <- tune_race_survival(
    racing_workflow(), resamples = folds, grid = 4,
    control = control_race_survival(burn_in = 2, randomize = FALSE)
  )
  expect_s3_class(res, "tune_results")
  expected <- default_eval_time(survival::Surv(vet$time, vet$status))
  expect_equal(tune::.get_tune_eval_times(res), expected)

  # an explicit eval_time wins
  res2 <- tune_race_survival(
    racing_workflow(), resamples = folds, grid = 4, eval_time = c(30, 90),
    control = control_race_survival(burn_in = 2, randomize = FALSE)
  )
  expect_equal(tune::.get_tune_eval_times(res2), c(30, 90))
})

test_that("the default evaluation times are read from a recipe's outcome too", {
  skip_if_no_racing_deps()
  skip_if_not_installed("recipes")

  vet <- stats::na.omit(survival::veteran[1:90, ])
  vet$surv <- survival::Surv(vet$time, vet$status)
  vet <- vet[c("surv", "karno", "diagtime", "age")]
  folds <- rsample::vfold_cv(vet, v = 3)
  times <- race_default_eval_time(
    workflows::workflow() |> workflows::add_recipe(recipes::recipe(surv ~ ., data = vet)),
    folds
  )
  expect_equal(times, default_eval_time(vet$surv))
})

test_that("race_default_eval_time says what to do when no Surv outcome can be read", {
  skip_if_no_racing_deps()

  folds <- rsample::vfold_cv(datasets::mtcars, v = 3)
  wflow <- workflows::workflow() |>
    workflows::add_formula(mpg ~ wt) |>
    workflows::add_model(parsnip::linear_reg())
  expect_error(race_default_eval_time(wflow, folds), "pass `eval_time`")
})

test_that("raced results work with the rest of tidymodels: select_best, show_best, fit_best, plot_race", {
  skip_if_no_racing_deps()
  skip_if_not_installed("ggplot2")

  vet <- stats::na.omit(survival::veteran[1:90, ])
  set.seed(1503)
  folds <- rsample::vfold_cv(vet, v = 3)
  res <- tune_race_survival(
    racing_workflow(), resamples = folds, grid = 5, eval_time = c(30, 90),
    control = control_race_survival(burn_in = 2, randomize = FALSE)
  )

  best <- tune::select_best(res, metric = "brier_survival_integrated")
  expect_true(is.numeric(best$penalty) && best$penalty > 0)
  shown <- tune::show_best(res, metric = "concordance_survival", n = 2)
  expect_true(nrow(shown) >= 1)
  expect_s3_class(finetune::plot_race(res), "ggplot")

  # control_race_survival() keeps the workflow, which is what fit_best() needs
  fit <- tune::fit_best(res, metric = "brier_survival_integrated")
  expect_s3_class(fit, "workflow")
  pred <- stats::predict(fit, vet[1:3, ], type = "time")
  expect_equal(nrow(pred), 3)
})

# ---- Racing inside the nested cross-validation functions ----------------------------------------

test_that("nested_cv_joint_model accepts penalty with the default tune_method", {
  skip_if_no_racing_deps()

  set.seed(42)
  n <- 120
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n), x2 = stats::rnorm(n)
  )
  folds <- rsample::nested_cv(df, outside = rsample::vfold_cv(v = 2), inside = rsample::vfold_cv(v = 2))
  # `penalty` reaches joint_model() through `...`; the internal call also names it, which used to
  # stop with 'formal argument "penalty" matched by multiple actual arguments'
  res <- nested_cv_joint_model(folds, survival::Surv(time, status) ~ x1 + x2, penalty = 0.05)
  expect_s3_class(res, "nested_cv_joint_model")
  expect_equal(nrow(res), 6)
  expect_true(all(is.finite(res$ibs)))
})

test_that("nested_cv_joint_model races the Cox penalty on the inner resamples", {
  skip_if_no_racing_deps()

  set.seed(42)
  n <- 150
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n), x2 = stats::rnorm(n)
  )
  folds <- rsample::nested_cv(df, outside = rsample::vfold_cv(v = 2), inside = rsample::vfold_cv(v = 3))
  res <- nested_cv_joint_model(
    folds, survival::Surv(time, status) ~ x1 + x2, tune_method = "race_anova"
  )
  expect_s3_class(res, "nested_cv_joint_model")
  expect_equal(nrow(res), 6)
  expect_true(all(is.finite(res$ibs)))
  expect_setequal(unique(res$model), c("coxnet", "status_calibrated", "time_regression"))

  # a penalty given as well is replaced by the raced one, and the caller is told
  expect_warning(
    nested_cv_joint_model(
      folds, survival::Surv(time, status) ~ x1 + x2, tune_method = "race_anova", penalty = 0.05
    ),
    "`penalty` is ignored"
  )
})

test_that("racing stops early, with the reason, on start/stop outcomes", {
  skip_if_no_racing_deps()

  set.seed(45)
  ns <- 40
  t_mid <- stats::runif(ns, 2, 5)
  t_end <- t_mid + stats::runif(ns, 2, 7)
  long <- data.frame(
    subject_id = rep(seq_len(ns), each = 2),
    tstart = as.vector(rbind(rep(0, ns), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, ns), stats::rbinom(ns, 1, 0.6))),
    x1 = stats::rnorm(2 * ns), x2 = stats::rnorm(2 * ns)
  )
  folds <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(
    nested_cv_joint_model(
      folds, survival::Surv(tstart, tstop, status) ~ x1 + x2,
      subject_id = "subject_id", tune_method = "race_anova"
    ),
    "needs a right-censored outcome"
  )
  expect_error(
    nested_cv_coxnet(
      folds, survival::Surv(tstart, tstop, status) ~ x1 + x2,
      subject_id = "subject_id", tune_method = "race_anova"
    ),
    "needs a right-censored outcome"
  )
})

test_that("win-loss racing asks for BradleyTerry2 up front", {
  skip_if_no_racing_deps()
  skip_if(requireNamespace("BradleyTerry2", quietly = TRUE), "BradleyTerry2 is installed")

  vet <- stats::na.omit(survival::veteran[1:60, ])
  folds <- rsample::vfold_cv(vet, v = 3)
  expect_error(
    tune_race_survival(racing_workflow(), resamples = folds, fn = "tune_race_win_loss"),
    "BradleyTerry2"
  )
})
