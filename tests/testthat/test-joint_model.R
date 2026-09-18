test_that("joint_model fits coxnet, status, and time models on 2-parameter data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  expect_s3_class(fit, "joint_model")
  expect_true(!is.null(fit$coxnet_model))
  expect_true(!is.null(fit$status_model))
  expect_true(!is.null(fit$time_model))

  # Test print
  expect_output(print(fit), "TempleCBE Joint Survival-Status-Time Model")

  # Test predict
  preds <- stats::predict(fit, new_data = df[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 5L)
  expect_true(all(c(".pred_survival", ".pred_status", ".pred_status_calibrated",
                    ".pred_time", ".pred_linear_pred", ".pred_risk_score") %in% names(preds)))

  # Test tidy
  td <- generics::tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("term", "estimate_coxnet", "estimate_status", "estimate_time") %in% names(td)))
  expect_equal(td$term, c("x1", "x2"))
})

test_that("joint_model supports counting-process start/stop data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(42)
  t_mid <- stats::runif(30, 2, 5)
  t_end <- t_mid + stats::runif(30, 2, 7)
  long <- data.frame(
    subject_id = rep(1:30, each = 2),
    tstart = as.vector(rbind(rep(0, 30), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, 30), stats::rbinom(30, 1, 0.6))),
    x1 = stats::rnorm(60),
    x2 = stats::rnorm(60)
  )

  fit <- joint_model(
    long,
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    subject_id = "subject_id",
    penalty = 0.05
  )
  expect_s3_class(fit, "joint_model")
  expect_equal(fit$components$type, "counting")
  expect_equal(length(fit$components$start), 60L)
})

test_that("joint_model supports baguette and stacks engines", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("stacks")
  skip_if_not_installed("parsnip")

  set.seed(99)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.5),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  # Baguette engine
  fit_bag <- joint_model(
    df,
    survival::Surv(time, status) ~ x1 + x2,
    engine = "baguette",
    penalty = 0.05
  )
  expect_s3_class(fit_bag, "joint_model")
  expect_s3_class(fit_bag$status_model, "model_fit")
  expect_s3_class(fit_bag$time_model, "model_fit")

  preds_bag <- stats::predict(fit_bag, new_data = df[1:4, ])
  expect_equal(nrow(preds_bag), 4L)

  # Stacks engine
  fit_stack <- joint_model(
    df,
    survival::Surv(time, status) ~ x1 + x2,
    engine = "stacks",
    penalty = 0.05
  )
  expect_s3_class(fit_stack, "joint_model")
  expect_true(!is.null(fit_stack$stack_model))
})

test_that("cv_joint_model computes cross-validated IBS and concordance", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(777)
  n <- 45
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  cv_res <- cv_joint_model(
    df,
    survival::Surv(time, status) ~ x1 + x2,
    v = 3,
    penalty = 0.05
  )

  expect_s3_class(cv_res, "cv_joint_model")
  expect_true(all(c("model", "ibs", "concordance", "fold") %in% names(cv_res)))
  expect_setequal(unique(cv_res$model), c("coxnet", "status_calibrated", "time_regression"))
  expect_true(all(is.finite(cv_res$ibs)))

  # Autoplot
  p <- ggplot2::autoplot(cv_res)
  expect_s3_class(p, "ggplot")
})
