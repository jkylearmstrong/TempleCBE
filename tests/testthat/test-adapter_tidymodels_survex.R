fake_prediction <- function(n, times, f = function(i, t) exp(-0.01 * i * t)) {
  tibble::tibble(.pred = lapply(seq_len(n), function(i) {
    tibble::tibble(.eval_time = times, .pred_survival = f(i, times))
  }))
}

local_fake_workflow <- function(pred_fun, env = parent.frame()) {
  obj <- structure(list(), class = "fake_survival_wf")
  registerS3method("predict", "fake_survival_wf", function(object, new_data, type, eval_time, ...) {
    stopifnot(identical(type, "survival"))
    pred_fun(nrow(new_data), eval_time)
  })
  obj
}

test_that("adapter_tidymodels_survex() turns the nested prediction tibble into an N x T matrix", {
  wf <- local_fake_workflow(function(n, t) fake_prediction(n, t))
  m <- adapter_tidymodels_survex(wf, data.frame(a = 1:3), c(5, 10, 20))
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(3L, 3L))
  expect_equal(colnames(m), c("5", "10", "20"))
  expect_equal(m[2, ], exp(-0.01 * 2 * c(5, 10, 20)), ignore_attr = TRUE)
  expect_true(all(m >= 0 & m <= 1))
})

test_that("rows follow new_data order and times are matched, not assumed to be in order", {
  wf <- local_fake_workflow(function(n, t) {
    p <- fake_prediction(n, rev(t))
    p
  })
  m <- adapter_tidymodels_survex(wf, data.frame(a = 1:2), c(5, 10))
  expect_equal(m[1, ], exp(-0.01 * 1 * c(5, 10)), ignore_attr = TRUE)
})

test_that("problems with the predictions are errors with a reason", {
  expect_error(adapter_tidymodels_survex(local_fake_workflow(function(n, t) tibble::tibble(x = 1:n)), data.frame(a = 1:2), 5), "\\.pred")
  expect_error(adapter_tidymodels_survex(local_fake_workflow(function(n, t) fake_prediction(n + 1L, t)), data.frame(a = 1:2), 5), "predictions for 2 rows")
  expect_error(adapter_tidymodels_survex(local_fake_workflow(function(n, t) fake_prediction(n, 99)), data.frame(a = 1), 5), "no prediction for time")
  expect_error(adapter_tidymodels_survex(local_fake_workflow(function(n, t) fake_prediction(n, t, function(i, t) rep(1.5, length(t)))), data.frame(a = 1), 5), "between 0 and 1")
  bad <- tibble::tibble(.pred = list(tibble::tibble(a = 1)))
  expect_error(adapter_tidymodels_survex(local_fake_workflow(function(n, t) bad), data.frame(a = 1), 5), "lacks")
})

test_that("times and new_data are validated", {
  wf <- local_fake_workflow(function(n, t) fake_prediction(n, t))
  expect_error(adapter_tidymodels_survex(wf, data.frame(a = 1), c(10, 5)), "increasing")
  expect_error(adapter_tidymodels_survex(wf, data.frame(a = 1), c(5, 5)), "increasing")
  expect_error(adapter_tidymodels_survex(wf, data.frame(a = 1), -1), "non-negative")
  expect_error(adapter_tidymodels_survex(wf, data.frame(a = 1), NA_real_), "finite")
  expect_error(adapter_tidymodels_survex(wf, data.frame(a = 1), numeric()), "non-negative")
  expect_error(adapter_tidymodels_survex(wf, 1:3, 5), "data frame")
})

test_that("a real censored-regression workflow gives a probability matrix", {
  skip_if_not_installed("censored")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$surv <- survival::Surv(lung$time, lung$status)
  wf <- workflows::workflow(surv ~ age + sex, parsnip::proportional_hazards() |> parsnip::set_engine("survival"))
  fit <- suppressWarnings(generics::fit(wf, data = lung))
  m <- adapter_tidymodels_survex(fit, lung[1:4, ], c(100, 300, 600))
  expect_equal(dim(m), c(4L, 3L))
  expect_true(all(m >= 0 & m <= 1))
  expect_true(all(m[, 1] >= m[, 3])) # survival does not increase
})
