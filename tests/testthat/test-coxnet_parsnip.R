skip_if_no_parsnip_engine <- function() {
  skip_if_no_coxnet_deps()
  for (pkg in c("parsnip", "workflows", "dials")) testthat::skip_if_not_installed(pkg)
}

coxnet_spec <- function(penalty = 0.05, mixture = 0.5) {
  parsnip::set_engine(parsnip::proportional_hazards(penalty = penalty, mixture = mixture), "coxnet")
}

test_that("the coxnet engine is registered for proportional_hazards()", {
  skip_if_no_parsnip_engine()
  engines <- parsnip::show_engines("proportional_hazards")
  expect_true(any(engines$engine == "coxnet" & engines$mode == "censored regression"))
  # Registering twice is a no-op, as when the package is reloaded.
  expect_true(register_coxnet_engine())
})

test_that("a parsnip fit reproduces coxnet() for linear predictors, survival, and time", {
  skip_if_no_parsnip_engine()
  d <- sim_counting()
  d <- d[!duplicated(d$subject_id), ]
  f <- survival::Surv(tstop, status) ~ x1 + x2 + x3

  fit <- parsnip::fit(coxnet_spec(), f, data = d)
  expect_s3_class(fit$fit, "coxnet_model")
  direct <- coxnet(f, data = d, penalty = 0.05, mixture = 0.5)

  nd <- d[1:6, ]
  expect_equal(predict(fit, nd, type = "linear_pred"), predict(direct, nd, type = "linear_pred"))
  expect_equal(predict(fit, nd, type = "time"), predict(direct, nd, type = "time"))
  expect_equal(
    predict(fit, nd, type = "survival", eval_time = c(3, 6)),
    predict(direct, nd, type = "survival", eval_time = c(3, 6))
  )
  expect_equal(generics::tidy(fit), generics::tidy(direct))
})

test_that("the engine accepts counting-process outcomes", {
  skip_if_no_parsnip_engine()
  d <- sim_counting()
  fit <- parsnip::fit(coxnet_spec(), survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3, data = d)
  expect_s3_class(fit$fit, "coxnet_model")
  expect_equal(nrow(predict(fit, d[1:3, ], type = "survival", eval_time = 5)), 3)
})

test_that("restricted mean survival time is the area under the predicted curve and orders by risk", {
  skip_if_no_coxnet_deps()
  d <- sim_counting()
  d <- d[!duplicated(d$subject_id), ]
  fit <- coxnet(survival::Surv(tstop, status) ~ x1 + x2 + x3, data = d, penalty = 0.05)
  nd <- d[1:5, ]
  rmst <- predict(fit, nd, type = "time")$.pred_time

  tmax <- max(d$tstop)
  grid <- seq(0, tmax, length.out = 20001)
  curves <- predict(fit, nd, type = "survival", eval_time = grid[-1])$.pred
  area <- vapply(curves, function(p) {
    s <- c(1, p$.pred_survival)
    sum(s[-length(s)] * diff(grid))   # left-endpoint rule: survival is a step function
  }, numeric(1))
  expect_equal(rmst, area, tolerance = 1e-2)
  expect_true(all(rmst > 0 & rmst <= tmax))

  # Higher linear predictor (as coxnet signs it) means longer survival.
  lp <- predict(fit, nd, type = "linear_pred")$.pred_linear_pred
  expect_equal(order(rmst), order(lp))
})

test_that("the engine rejects a missing penalty and case weights with clear errors", {
  skip_if_no_parsnip_engine()
  d <- sim_counting()
  d <- d[!duplicated(d$subject_id), ]
  f <- survival::Surv(tstop, status) ~ x1 + x2 + x3
  expect_error(
    parsnip::fit(parsnip::set_engine(parsnip::proportional_hazards(), "coxnet"), f, data = d),
    "needs a `penalty`"
  )
  expect_error(coxnet_train(f, d, penalty = 0.1, weights = rep(1, nrow(d))), "case weights")
})

test_that("coxnet workflows can be tuned and match censored's glmnet engine", {
  skip_if_no_parsnip_engine()
  for (pkg in c("tune", "rsample")) testthat::skip_if_not_installed(pkg)
  d <- sim_counting(n = 120)
  d <- d[!duplicated(d$subject_id), ]
  f <- survival::Surv(tstop, status) ~ x1 + x2 + x3

  # tune() must appear literally in the call for parsnip to detect it.
  wf <- workflows::workflow(
    f,
    parsnip::set_engine(parsnip::proportional_hazards(penalty = tune::tune(), mixture = 0.5), "coxnet")
  )
  res <- suppressMessages(tune::tune_grid(
    wf, rsample::vfold_cv(d, v = 3),
    grid = tibble::tibble(penalty = c(0.01, 0.1)),
    metrics = yardstick::metric_set(yardstick::concordance_survival)
  ))
  metrics <- tune::collect_metrics(res)
  expect_equal(sort(metrics$penalty), c(0.01, 0.1))
  expect_true(all(metrics$mean >= 0 & metrics$mean <= 1))

  testthat::skip_if_not_installed("censored")
  ref <- suppressWarnings(parsnip::fit(
    parsnip::set_engine(parsnip::proportional_hazards(penalty = 0.05, mixture = 0.5), "glmnet"),
    f, data = d
  ))
  ours <- parsnip::fit(coxnet_spec(), f, data = d)
  nd <- d[1:8, ]
  expect_equal(
    predict(ours, nd, type = "linear_pred")$.pred_linear_pred,
    predict(ref, nd, type = "linear_pred")$.pred_linear_pred,
    tolerance = 1e-6
  )
  # Baseline hazard estimators differ slightly (Breslow here, survfit in censored).
  expect_equal(
    predict(ours, nd, type = "survival", eval_time = c(3, 6))$.pred[[1]]$.pred_survival,
    predict(ref, nd, type = "survival", eval_time = c(3, 6))$.pred[[1]]$.pred_survival,
    tolerance = 5e-3
  )
})
