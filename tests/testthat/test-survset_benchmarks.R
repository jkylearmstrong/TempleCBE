# SurvSet Benchmark Test Suite (tests/testthat/test-survset_benchmarks.R)
# Validates TempleCBE survival routines, joint modeling engines, explainability,
# and racing methods against benchmark cohorts from SurvSet (Drysdale et al. 2022).

skip_if_no_survset_deps <- function() {
  for (pkg in c("survival", "glmnet", "recipes", "hardhat", "rsample", "yardstick")) {
    testthat::skip_if_not_installed(pkg)
  }
}

test_that("SurvSet veteran cohort fits coxnet and cv_coxnet with expected properties", {
  skip_if_no_survset_deps()
  vet <- load_survset_benchmark("veteran")

  # Standard coxnet fit
  fit <- coxnet(survival::Surv(time, event) ~ trt + celltype + karno + diagtime + age + prior, data = vet, penalty = 0.05)
  expect_s3_class(fit, "coxnet_model")
  expect_s3_class(fit, "hardhat_model")

  preds_lp <- predict(fit, vet, type = "linear_pred")
  expect_equal(nrow(preds_lp), nrow(vet))
  expect_true(all(is.finite(preds_lp$.pred_linear_pred)))

  preds_surv <- predict(fit, vet, type = "survival", eval_time = c(30, 90, 180))
  expect_equal(nrow(preds_surv), nrow(vet))
  expect_equal(nrow(preds_surv$.pred[[1]]), 3)

  # Cross-validated coxnet on veteran
  set.seed(42)
  cv_fit <- cv_coxnet(
    survival::Surv(time, event) ~ trt + celltype + karno + diagtime + age + prior,
    data = vet,
    v = 3
  )
  expect_s3_class(cv_fit, "cv_coxnet")
  expect_true(is.numeric(cv_fit$lambda_min) && cv_fit$lambda_min > 0)
  expect_true("brier_survival_integrated" %in% cv_fit$metrics$.metric)
})

test_that("SurvSet lung cohort works with canonical km_single, cox_single, cox_multi, and cox_table", {
  skip_if_no_survset_deps()
  lung <- load_survset_benchmark("lung")

  # Canonical km_single
  km <- km_single(lung, outcome = "survival::Surv(time, event)", feature = "sex")
  expect_s3_class(km, "cbe_km")
  expect_true(all(c("cox", "km_fit", "summary") %in% names(km)))

  # Canonical cox_single
  cx <- cox_single(lung, outcome = "survival::Surv(time, event)", feature = "age")
  expect_s3_class(cx, "cbe_cox")
  expect_true(!is.null(cx$table))

  # Canonical cox_multi + cox_check + cox_table
  multi <- cox_multi(lung, formula = survival::Surv(time, event) ~ age + sex + ph.ecog)
  expect_s3_class(multi, "cbe_cox_multi")

  chk <- cox_check(multi)
  expect_s3_class(chk, "cbe_cox_check")
  expect_true(is.character(chk$zph_text))

  tbl <- cox_table(multi, sort = "magnitude", significance = TRUE)
  expect_s3_class(tbl, "tbl_df")
  expect_true(all(c("Variable", "HR", "log(HR)", "sig") %in% names(tbl)))
})

test_that("SurvSet lung cohort benchmarks joint_model across glmnet, baguette, and stacks engines", {
  skip_if_no_survset_deps()
  lung <- load_survset_benchmark("lung")

  # 1. glmnet engine
  jm_glmnet <- joint_model(
    data = lung,
    outcome = survival::Surv(time, event) ~ age + sex + ph.karno,
    engine = "glmnet",
    penalty = 0.05,
    calibration = FALSE
  )
  expect_s3_class(jm_glmnet, "joint_model")
  p_glmnet <- predict(jm_glmnet, lung, type = "survival", eval_time = c(100, 200))
  expect_equal(nrow(p_glmnet), nrow(lung))

  # 2. baguette engine
  skip_if_not_installed("baguette")
  jm_baguette <- joint_model(
    data = lung,
    outcome = survival::Surv(time, event) ~ age + sex + ph.karno,
    engine = "baguette",
    penalty = 0.05,
    calibration = FALSE
  )
  expect_s3_class(jm_baguette, "joint_model")
  p_time <- predict(jm_baguette, lung, type = "time")
  expect_equal(nrow(p_time), nrow(lung))
  expect_true(all(is.finite(p_time$.pred_time)))

  # 3. stacks engine
  jm_stacks <- suppressWarnings(joint_model(
    data = lung,
    outcome = survival::Surv(time, event) ~ age + sex + ph.karno,
    engine = "stacks",
    penalty = 0.05,
    calibration = FALSE
  ))
  expect_s3_class(jm_stacks, "joint_model")
  expect_true(!is.null(jm_stacks$stack_model))
  expect_true(!is.null(jm_stacks$stack_scales))

  p_stack_risk <- predict(jm_stacks, lung, type = "stack_risk_score")
  expect_equal(nrow(p_stack_risk), nrow(lung))
  expect_true(all(is.finite(p_stack_risk$.pred_stack_risk_score)))
})

test_that("SurvSet benchmark integrates with DALEX and Shapley explainability", {
  skip_if_no_survset_deps()
  skip_if_not_installed("DALEX")
  skip_if_not_installed("survex")

  lung <- load_survset_benchmark("lung")
  lung_sub <- lung[1:80, ]

  jm <- joint_model(
    data = lung_sub,
    outcome = survival::Surv(time, event) ~ age + sex + ph.karno,
    engine = "glmnet",
    penalty = 0.05,
    calibration = FALSE
  )

  exp_all <- explain_joint(jm, data = lung_sub, verbose = FALSE)
  expect_true(all(c("survival", "status", "time") %in% names(exp_all)))
  expect_s3_class(exp_all$survival, "surv_explainer")
  expect_s3_class(exp_all$status, "explainer")
  expect_s3_class(exp_all$time, "explainer")

  # Compute SHAP values on status model
  shap_status <- predict_parts_shap(exp_all$status, new_observation = lung_sub[1, ], B = 10)
  expect_s3_class(shap_status, "predict_parts")
  expect_true(nrow(shap_status) > 0)
})

test_that("SurvSet heart counting-process cohort benchmarks start/stop survival models", {
  skip_if_no_survset_deps()
  heart <- load_survset_benchmark("heart")

  # Fit coxnet on start/stop counting process
  fit_cp <- coxnet(
    survival::Surv(tstart, tstop, event) ~ age + surgery + transplant,
    data = heart,
    penalty = 0.05
  )
  expect_s3_class(fit_cp, "coxnet_model")
  expect_equal(attr(fit_cp$y, "type"), "counting")

  pred_lp <- predict(fit_cp, heart, type = "linear_pred")
  expect_equal(nrow(pred_lp), nrow(heart))

  # Fit joint_model on start/stop with subject_id
  jm_cp <- joint_model(
    data = heart,
    outcome = survival::Surv(tstart, tstop, event) ~ age + surgery + transplant,
    subject_id = "pid",
    engine = "glmnet",
    penalty = 0.05,
    calibration = FALSE
  )
  expect_s3_class(jm_cp, "joint_model")
  expect_equal(jm_cp$components$subject_id, "pid")

  pred_time <- predict(jm_cp, heart, type = "time")
  expect_equal(nrow(pred_time), nrow(heart))
})

test_that("SurvSet veteran cohort tunes via finetune racing anova and win-fraction", {
  skip_if_no_survset_deps()
  for (pkg in c("finetune", "parsnip", "workflows")) {
    testthat::skip_if_not_installed(pkg)
  }

  vet <- load_survset_benchmark("veteran")
  set.seed(1503)
  folds <- rsample::vfold_cv(vet, v = 3)

  spec <- parsnip::set_engine(
    parsnip::proportional_hazards(penalty = tune::tune(), mixture = 1),
    "coxnet"
  ) |> parsnip::set_mode("censored regression")

  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(survival::Surv(time, event) ~ karno + age + diagtime)

  ctrl <- control_race_survival(burn_in = 2, randomize = FALSE)
  expect_s3_class(ctrl, "control_race")

  race_res <- tune_race_survival(
    wflow,
    resamples = folds,
    fn = "tune_race_anova",
    grid = 6,
    eval_time = c(60, 180),
    control = ctrl,
    seed = 1503
  )
  expect_s3_class(race_res, "tune_results")

  best <- tune::select_best(race_res, metric = "brier_survival_integrated")
  expect_true("penalty" %in% names(best))
  expect_true(is.numeric(best$penalty) && best$penalty > 0)
})
