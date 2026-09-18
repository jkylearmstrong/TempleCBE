test_that("cbe_explain_survival works with joint_model and coxnet_model", {
  skip_if_not_installed("survex")
  skip_if_not_installed("DALEX")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.7),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  expl <- cbe_explain_survival(fit)

  expect_s3_class(expl, "surv_explainer")
  expect_true(!is.null(expl$predict_survival_function))
  expect_true(!is.null(expl$predict_risk_function))

  # Also test with coxnet_model directly
  expl_cox <- cbe_explain_survival(
    fit$coxnet_model,
    data = df[, c("x1", "x2")],
    y = survival::Surv(df$time, df$status)
  )
  expect_s3_class(expl_cox, "surv_explainer")
})

test_that("cbe_explain_survival works with tidymodels workflow and parsnip (Issue #98)", {
  skip_if_not_installed("survex")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("survival")

  set.seed(42)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.7),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  c_fit <- survival::coxph(survival::Surv(time, status) ~ x1 + x2, data = df, x = TRUE)
  expl_c <- cbe_explain_survival(
    c_fit,
    data = df[, c("x1", "x2")],
    y = survival::Surv(df$time, df$status)
  )
  expect_s3_class(expl_c, "surv_explainer")

  surv_preds <- expl_c$predict_survival_function(c_fit, df[1:5, c("x1", "x2")], times = expl_c$times[1:3])
  expect_equal(nrow(surv_preds), 5L)
  expect_equal(ncol(surv_preds), 3L)
})

test_that("cbe_survex_loss_ibs and cbe_survex_loss_brier integrate with survex", {
  skip_if_not_installed("survex")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(99)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.2,
    status = rep(c(0, 1), each = 30),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  expl <- cbe_explain_survival(fit)

  loss_ibs <- cbe_survex_loss_ibs()
  expect_equal(attr(loss_ibs, "loss_type"), "integrated")
  expect_equal(attr(loss_ibs, "loss_name"), "TempleCBE Integrated Brier Score")

  loss_brier <- cbe_survex_loss_brier()
  expect_equal(attr(loss_brier, "loss_type"), "time-dependent")
  expect_equal(attr(loss_brier, "loss_name"), "TempleCBE Brier Score")

  # Test survex::model_performance with custom loss
  perf <- survex::model_performance(expl, loss_function = loss_ibs)
  expect_s3_class(perf, "surv_model_performance")

  # Test survex::model_parts with custom loss
  mp <- survex::model_parts(expl, loss_function = loss_ibs, N = 20)
  expect_s3_class(mp, "model_parts_survival")
})

test_that("cbe_explain creates unified bundle and individual explainers", {
  skip_if_not_installed("survex")
  skip_if_not_installed("DALEX")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(456)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = rep(c(0, 1), each = 30),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)

  # Full bundle
  bundle <- cbe_explain(fit, type = "all")
  expect_s3_class(bundle, "cbe_joint_explainer")
  expect_s3_class(bundle$survival, "surv_explainer")
  expect_s3_class(bundle$status, "explainer")
  expect_s3_class(bundle$time, "explainer")

  # Helper functions
  expl_stat <- cbe_explain_status(fit)
  expect_s3_class(expl_stat, "explainer")

  expl_tm <- cbe_explain_time(fit)
  expect_s3_class(expl_tm, "explainer")
})

test_that("cbe_predict_parts_shap works on survival and joint explainers", {
  skip_if_not_installed("survex")
  skip_if_not_installed("DALEX")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(789)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = rep(c(0, 1), each = 25),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  new_obs <- df[1, c("x1", "x2")]

  # Survival explainer SurvSHAP
  expl_surv <- cbe_explain_survival(fit)
  shap_surv <- cbe_predict_parts_shap(expl_surv, new_observation = new_obs, type = "survshap", N = 15)
  expect_s3_class(shap_surv, "predict_parts_survival")

  # Status explainer standard SHAP
  expl_stat <- cbe_explain_status(fit)
  shap_stat <- cbe_predict_parts_shap(expl_stat, new_observation = new_obs, type = "shap", B = 10)
  expect_s3_class(shap_stat, "predict_parts")

  # Joint explainer bundle
  bundle <- cbe_explain(fit, type = "all")
  joint_shap <- cbe_predict_parts_shap(bundle, new_observation = new_obs, N = 15, B = 10)
  expect_s3_class(joint_shap, "cbe_joint_predict_parts")
  expect_true(!is.null(joint_shap$survival))
  expect_true(!is.null(joint_shap$status))
  expect_true(!is.null(joint_shap$time))
})
