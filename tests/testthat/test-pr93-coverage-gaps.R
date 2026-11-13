# Coverage for the branches Codecov reported as untested in the PR #93 patch
# (0.9.0 round-3 fixes): the alignment check behind `n_imputed` in
# imputation_sensitivity() and two paths in R/predict_calibration.R.

# --- .observed_cells_aligned(): direct unit tests ------------------------------

test_that(".observed_cells_aligned() skips a column with no observed cells", {
  # nothing observed in `data`, so nothing can have changed
  expect_true(.observed_cells_aligned(
    data.frame(a = c(1, 2)), data.frame(a = c(NA_real_, NA_real_))
  ))
  expect_true(.observed_cells_aligned(
    data.frame(a = c(NA, NA), b = c(1, 2)), data.frame(a = c(NA, NA), b = c(1, 2))
  ))
  # a later column is still checked after the skipped one
  expect_false(.observed_cells_aligned(
    data.frame(a = c(1, 2), b = c(1, 3)), data.frame(a = c(NA_real_, NA_real_), b = c(1, 2))
  ))
})

test_that(".observed_cells_aligned() is FALSE when an observed cell becomes NA", {
  expect_false(.observed_cells_aligned(
    data.frame(a = c(NA, 2)), data.frame(a = c(1, 2))
  ))
  expect_false(.observed_cells_aligned(
    data.frame(g = c("a", NA)), data.frame(g = c("a", "b"))
  ))
})

test_that(".observed_cells_aligned() compares non-numeric columns as character", {
  # observed cells unchanged, the missing one filled: aligned
  expect_true(.observed_cells_aligned(
    data.frame(g = c("a", "b")), data.frame(g = c("a", NA))
  ))
  # an observed value changed: not aligned
  expect_false(.observed_cells_aligned(
    data.frame(g = c("b", "b")), data.frame(g = c("a", NA))
  ))
  # factor vs character with the same labels is aligned
  expect_true(.observed_cells_aligned(
    data.frame(g = factor(c("a", "b"))), data.frame(g = c("a", NA))
  ))
  expect_false(.observed_cells_aligned(
    data.frame(g = factor(c("b", "b"))), data.frame(g = c("a", NA))
  ))
  # logical columns take the same path
  expect_true(.observed_cells_aligned(
    data.frame(l = c(TRUE, FALSE)), data.frame(l = c(TRUE, NA))
  ))
  expect_false(.observed_cells_aligned(
    data.frame(l = c(FALSE, FALSE)), data.frame(l = c(TRUE, NA))
  ))
})

# --- the same branches through imputation_sensitivity() -------------------------

# y and g each have one missing cell; z is missing throughout.
pr93_mixed_data <- function() {
  data.frame(
    y = c(1, 2, NA, 4, 5, 6, 7, 8),
    x = c(2, 1, 3, 5, 4, 6, 8, 7),
    g = c("a", "b", "a", NA, "b", "a", "b", "a"),
    z = rep(NA_real_, 8),
    stringsAsFactors = FALSE
  )
}

pr93_n_imputed <- function(strategy) {
  res <- suppressWarnings(imputation_sensitivity(
    pr93_mixed_data(),
    fit_fn = function(x) stats::lm(y ~ x, data = x),
    strategies = list(median = "median", s = strategy)
  ))
  res$n_imputed$n_imputed[res$n_imputed$strategy == "s"]
}

test_that("n_imputed counts filled character cells and ignores an all-NA column", {
  res <- suppressWarnings(imputation_sensitivity(
    pr93_mixed_data(),
    fit_fn = function(x) stats::lm(y ~ x, data = x),
    strategies = list(median = "median", mean = "mean")
  ))
  # y and g are filled; z has nothing to summarise and stays NA
  expect_equal(res$n_imputed$n_imputed, c(2L, 2L))
  # a function strategy that fills the same cells is counted the same way
  expect_equal(pr93_n_imputed(function(x) {
    x$y[is.na(x$y)] <- 0
    x$g[is.na(x$g)] <- "b"
    x
  }), 2L)
})

test_that("n_imputed is NA when a strategy changes an observed character cell", {
  expect_true(is.na(pr93_n_imputed(function(x) {
    x$y[is.na(x$y)] <- 0
    x$g[is.na(x$g)] <- "a"
    x$g[1] <- "b"
    x
  })))
})

test_that("n_imputed is NA when a strategy blanks an observed cell", {
  expect_true(is.na(pr93_n_imputed(function(x) {
    x$y[is.na(x$y)] <- 0
    x$x[1] <- NA
    x
  })))
})

# --- extract_horizon_risk() / predict_event_risk() ------------------------------

test_that("extract_horizon_risk() reads a top-level .pred_cif list-column", {
  curve <- tibble::tibble(.eval_time = c(5, 10), .pred_cif = c(0.1, 0.3))
  pr <- tibble::tibble(.pred_cif = list(curve, curve))
  expect_equal(extract_horizon_risk(pr, 10), c(0.3, 0.3))
  expect_equal(extract_horizon_risk(pr, 5), c(0.1, 0.1))
})

test_that("extract_horizon_risk() reads a top-level .pred_survival list-column", {
  curve <- tibble::tibble(.eval_time = c(5, 10), .pred_survival = c(0.9, 0.6))
  pr <- tibble::tibble(.pred_survival = list(curve, curve))
  expect_equal(extract_horizon_risk(pr, 10), c(0.4, 0.4))
})

test_that("predict_event_risk() returns all-NA risk when no cox_time_varying row can be scored", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d <- d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
  d$status <- d$status - 1
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ sex + age, tv_vars = "age")
  nd <- d[1:3, ]
  nd$age <- NA_real_
  risk <- predict_event_risk(fit, nd, horizon = 200)
  expect_length(risk, 3L)
  expect_true(all(is.na(risk)))
})
