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

# Graf IPCW Brier score, one subject and one time at a time, straight from the
# definition: no vectorized lookups, so no chance of pairing weights and
# subjects up wrongly.
naive_graf_brier <- function(time, status, surv, eval_time, trunc = 0.05) {
  cens <- survival::survfit(survival::Surv(time, 1 - status) ~ 1)
  g <- function(u) {
    max(summary(cens, times = u, extend = TRUE)$surv, trunc)
  }
  eps <- 1e-6  # the times below have two decimals
  vapply(seq_along(eval_time), function(j) {
    t <- eval_time[j]
    terms <- vapply(seq_along(time), function(i) {
      if (time[i] > t) {
        (1 - surv[i, j])^2 / g(t)
      } else if (status[i] == 1) {
        surv[i, j]^2 / g(time[i] - eps)
      } else {
        0
      }
    }, numeric(1))
    sum(terms) / length(time)
  }, numeric(1))
}

test_that("cbe_survex_loss_brier is the Graf IPCW Brier score and doesn't depend on row order", {
  # Regression test: the censoring probabilities came from summary.survfit(times = ),
  # which sorts `times`, so they were attached to the wrong subjects.
  skip_if_not_installed("survival")
  set.seed(3)
  n <- 40
  time <- round(stats::rexp(n, 0.1) + 0.2, 2)
  status <- stats::rbinom(n, 1, 0.6)
  surv <- matrix(stats::runif(n * 3, 0.2, 0.9), n, 3)
  times <- c(2, 5, 9)
  y <- survival::Surv(time, status)

  loss <- cbe_survex_loss_brier(trunc = 0.05)
  got <- loss(y_true = y, surv = surv, times = times)
  expect_length(got, 3)
  expect_equal(got, naive_graf_brier(time, status, surv, times))

  perm <- sample(n)
  expect_equal(loss(y_true = y[perm], surv = surv[perm, ], times = times), got)

  expect_equal(loss(y_true = y, surv = as.data.frame(surv), times = times), got)
  expect_error(loss(y_true = survival::Surv(c(0, 1, 2), c(1, 2, 3), c(1, 0, 1)), surv = surv[1:3, ], times = times), "right-censored")
})

test_that("explain_predict_risk scores new data, with higher meaning higher risk", {
  skip_if_not_installed("survival")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  set.seed(1)
  n <- 80
  df <- data.frame(time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.7), x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  df$time <- df$time * exp(-df$x1)  # x1 raises the hazard

  cox <- survival::coxph(survival::Surv(time, status) ~ x1 + x2, data = df)
  new <- df[1:7, c("x1", "x2")]
  risk <- explain_predict_risk(cox, new)
  # predict.coxph() has no `new_data` argument: it used to score the 80 training rows instead.
  expect_length(risk, 7)
  expect_equal(risk, unname(stats::predict(cox, newdata = new, type = "lp")))

  net <- coxnet(survival::Surv(time, status) ~ x1 + x2, data = df, penalty = 0.01)
  net_risk <- explain_predict_risk(net, df[1:20, ])
  expect_length(net_risk, 20)
  expect_gt(stats::cor(net_risk, df$x1[1:20]), 0)  # the tidymodels default sign is the reverse
  expect_equal(net_risk, -predict(net, df[1:20, ])$.pred_linear_pred)

  expect_null(explain_predict_risk(structure(list(), class = "unknown_model"), new))
})

test_that("cbe_explain_survival refuses start/stop outcomes that survex would misread", {
  skip_if_not_installed("survex")
  skip_if_not_installed("survival")
  d <- sim_counting(n = 30)
  expect_error(
    cbe_explain_survival(
      survival::coxph(survival::Surv(tstart, tstop, status) ~ x1, data = d),
      data = d[c("x1")], y = survival::Surv(d$tstart, d$tstop, d$status)
    ),
    "start/stop"
  )
})

test_that("explain_predict_risk returns a risk score, not tidymodels' increasing linear predictor, for parsnip and workflow fits", {
  # Regression test: parsnip's censored-regression linear_pred defaults to
  # increasing with survival time (the opposite sign of the engine's own), and
  # the risk function used it as-is, so survex's risk-based performance and
  # importance were computed with the wrong sign.
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("censored")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")
  loadNamespace("censored")  # registers the "survival" and "glmnet" engines

  set.seed(1)
  n <- 120
  df <- data.frame(time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.75), x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  df$time <- df$time * exp(-df$x1)  # x1 raises the hazard
  f <- survival::Surv(time, status) ~ x1 + x2
  new <- df[1:15, ]
  cox <- survival::coxph(f, data = df)
  ref <- unname(stats::predict(cox, newdata = new, type = "lp"))  # engine sign: higher = higher risk

  # Compared up to a constant: censored's "survival" engine doesn't center the
  # linear predictor as predict.coxph() does, which is irrelevant to a risk ranking.
  centered <- function(x) x - mean(x)
  spec <- parsnip::set_engine(parsnip::proportional_hazards(), "survival")
  fit <- parsnip::fit(spec, f, data = df)
  # tidymodels' documented default: the linear predictor increases with survival time.
  expect_equal(centered(predict(fit, new, type = "linear_pred")$.pred_linear_pred), centered(-ref), tolerance = 1e-6)
  expect_equal(centered(explain_predict_risk(fit, new)), centered(ref), tolerance = 1e-6)

  wf <- parsnip::fit(workflows::workflow(f, spec), data = df)
  expect_equal(centered(explain_predict_risk(wf, new)), centered(ref), tolerance = 1e-6)
  expect_gt(stats::cor(explain_predict_risk(wf, new), ref), 0.999)

  penalized <- parsnip::proportional_hazards(penalty = 0.01, mixture = 1)
  glmnet_fit <- suppressWarnings(parsnip::fit(parsnip::set_engine(penalized, "glmnet"), f, data = df))
  expect_gt(stats::cor(explain_predict_risk(glmnet_fit, new), ref), 0.95)
  coxnet_fit <- parsnip::fit(parsnip::set_engine(penalized, "coxnet"), f, data = df)
  expect_gt(stats::cor(explain_predict_risk(coxnet_fit, new), ref), 0.95)
  # Same sign convention as the direct coxnet() model.
  direct <- coxnet(f, data = df, penalty = 0.01)
  expect_equal(explain_predict_risk(coxnet_fit, new), explain_predict_risk(direct, new), tolerance = 1e-6)
})
