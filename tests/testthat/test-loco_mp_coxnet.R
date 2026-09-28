test_that("cbe_loco_mp_coxnet errors clearly on a single eval_time instead of silently returning zero", {
  # Regression test: a single eval_time can't be integrated over (there's no
  # interval), and used to silently produce all-zero importance scores
  # instead of an error - both because check_eval_time() was never called,
  # and because vapply() collapsing to a plain vector (instead of a 1-row
  # matrix) fed a dimension mismatch further downstream.
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(789)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.65),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )

  expect_error(
    cbe_loco_mp_coxnet(
      survival::Surv(time, status) ~ x1 + x2 + x3,
      data = df,
      eval_time = 5,
      B = 15,
      n_ratio = 0.7,
      m_ratio = 0.5,
      seed = 42
    ),
    "at least 2 distinct"
  )
})

test_that("cbe_loco_mp_coxnet's OOB survival reshape keeps patches on the right axis for 2 eval_times", {
  # Regression test: with exactly 2 eval_times, vapply() still returns a
  # proper n_eval x length(with_sub) matrix on its own, but this locks in
  # that rowMeans() is averaging across patches (columns), not across time
  # points (rows), which the n_eval == 1 case had silently gotten backwards.
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(101)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.65),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )

  fit <- cbe_loco_mp_coxnet(
    survival::Surv(time, status) ~ x1 + x2 + x3,
    data = df,
    eval_time = c(2, 8),
    B = 15,
    n_ratio = 0.7,
    m_ratio = 0.5,
    seed = 42
  )

  expect_equal(nrow(fit$results), 3L)
  expect_true(any(!is.na(fit$results$importance) & fit$results$importance != 0))
})

test_that("cbe_loco_mp_coxnet works with 2-parameter Surv data via formula", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.65),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )

  fit <- cbe_loco_mp_coxnet(
    survival::Surv(time, status) ~ x1 + x2 + x3,
    data = df,
    B = 15,
    n_ratio = 0.7,
    m_ratio = 0.5,
    seed = 42
  )

  expect_s3_class(fit, "cbe_loco_mp_coxnet")
  expect_true(fit$B >= 5)
  expect_equal(nrow(fit$results), 3L)
  expect_true(all(c("term", "importance", "std_error", "statistic", "p_value", "p_adjusted", "conf_low", "conf_high") %in% names(fit$results)))

  # Print method
  expect_output(print(fit), "<cbe_loco_mp_coxnet>")

  # Tidy method
  td <- generics::tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_equal(nrow(td), 3L)

  # Autoplot method
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::autoplot(fit)
    expect_s3_class(p, "ggplot")
  }
})

test_that("cbe_loco_mp_coxnet works with x and y matrix interface", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(456)
  n <- 40
  x_mat <- matrix(stats::rnorm(n * 3), nrow = n, dimnames = list(NULL, c("feat_a", "feat_b", "feat_c")))
  y_surv <- survival::Surv(stats::rexp(n, 0.1) + 0.2, stats::rbinom(n, 1, 0.7))

  fit <- cbe_loco_mp_coxnet(
    x = x_mat,
    y = y_surv,
    B = 10,
    n_ratio = 0.75,
    m_ratio = 0.5,
    seed = 99
  )

  expect_s3_class(fit, "cbe_loco_mp_coxnet")
  expect_equal(fit$results$term, c("feat_a", "feat_b", "feat_c")[match(fit$results$term, c("feat_a", "feat_b", "feat_c"))])
})

test_that("cbe_loco_mp_coxnet works with start/stop data and subject_id", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(789)
  long <- do.call(rbind, lapply(1:30, function(i) {
    k <- 2
    stops <- cumsum(stats::runif(k, 2, 5))
    data.frame(
      subject_id = i,
      tstart = c(0, stops[1]),
      tstop = stops,
      status = c(0, stats::rbinom(1, 1, 0.6)),
      x1 = stats::rnorm(k),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))

  fit <- cbe_loco_mp_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    data = long,
    subject_id = "subject_id",
    B = 12,
    seed = 101
  )

  expect_s3_class(fit, "cbe_loco_mp_coxnet")
  expect_equal(nrow(fit$results), 3L)
  expect_true(is.numeric(fit$results$importance))
})

test_that("nested_cv_coxnet integrates importance = 'loco_mp'", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(111)
  n <- 45
  df <- data.frame(
    subject_id = seq_len(n),
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )

  folds <- rsample::nested_cv(
    df,
    outside = rsample::vfold_cv(v = 2),
    inside = rsample::vfold_cv(v = 2)
  )

  res <- nested_cv_coxnet(
    folds,
    survival::Surv(time, status) ~ x1 + x2 + x3,
    mixture = 1,
    importance = "loco_mp"
  )

  expect_s3_class(res, "nested_cv_coxnet")
  expect_true(".importance" %in% names(res))
  expect_s3_class(res$.importance[[1]], "tbl_df")
  expect_true(all(c("term", "importance", "p_value") %in% names(res$.importance[[1]])))
})

loco_start_stop_data <- function(n = 70, seed = 4) {
  d <- sim_counting(n = n, seed = seed)
  set.seed(seed + 1)
  d$x4 <- stats::rnorm(nrow(d))
  d$x5 <- stats::rnorm(nrow(d))
  d$subject_id <- paste0("s", d$subject_id)  # "s10" sorts before "s2"
  d
}

test_that("cbe_loco_mp_coxnet doesn't depend on the order of the rows or subjects in `data`", {
  # Regression test: subject weights were indexed by order of first appearance
  # but the subject-level truth is sorted by subject, so unsorted data paired
  # subjects with another subject's censoring weight and outcome.
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")

  d <- loco_start_stop_data()
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + x4 + x5
  sorted <- d[order(d$subject_id, d$tstart), ]
  set.seed(1)
  shuffled <- do.call(rbind, split(d, factor(d$subject_id, levels = sample(unique(d$subject_id)))))
  shuffled <- shuffled[sample(nrow(shuffled)), ]

  a <- cbe_loco_mp_coxnet(f, sorted, subject_id = "subject_id", B = 25, seed = 9, penalty = 0.02)
  b <- cbe_loco_mp_coxnet(f, shuffled, subject_id = "subject_id", B = 25, seed = 9, penalty = 0.02)
  expect_equal(a$results, b$results, tolerance = 1e-6)
})

test_that("cbe_loco_mp_coxnet's covariates = 'path' and 'baseline' agree for right-censored data and differ for start/stop data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")

  d <- loco_start_stop_data()
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3 + x4 + x5
  run <- function(data, covariates, formula = f, ...) {
    cbe_loco_mp_coxnet(formula, data, B = 20, seed = 3, penalty = 0.02, covariates = covariates, ...)
  }
  ss_path <- run(d, "path", subject_id = "subject_id")
  ss_base <- run(d, "baseline", subject_id = "subject_id")
  expect_equal(ss_path$covariates, "path")
  expect_equal(ss_base$covariates, "baseline")
  expect_false(isTRUE(all.equal(ss_path$results$importance, ss_base$results$importance)))

  first <- d[!duplicated(d$subject_id), ]
  right <- survival::Surv(tstop, status) ~ x1 + x2 + x3 + x4 + x5
  expect_equal(run(first, "path", right)$results, run(first, "baseline", right)$results)
  expect_error(run(d, "last"), "should be one of")
})

test_that("cbe_loco_mp_coxnet finds the covariate that drives the hazard, with x/y and formula inputs alike", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")

  set.seed(21)
  n <- 150
  x <- matrix(stats::rnorm(n * 4), n, dimnames = list(NULL, paste0("x", 1:4)))
  time <- stats::rexp(n, exp(1.5 * x[, "x1"]) / 10)
  status <- stats::rbinom(n, 1, 0.85)
  fit <- cbe_loco_mp_coxnet(x = x, y = survival::Surv(time, status), B = 40, seed = 1, penalty = 0.02)
  expect_equal(fit$results$term[1], "x1")
  expect_lt(fit$results$p_adjusted[fit$results$term == "x1"], 0.01)
  expect_true(all(fit$results$importance[fit$results$term != "x1"] < fit$results$importance[1]))
})

test_that("cbe_loco_mp_coxnet keeps two predictors per patch and validates the subject count", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("recipes")
  skip_if_not_installed("hardhat")

  set.seed(5)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.65),
    x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n)
  )
  # m_ratio * 3 rounds to 1 predictor, which coxnet() can't fit; patches keep two.
  fit <- cbe_loco_mp_coxnet(survival::Surv(time, status) ~ x1 + x2 + x3, df, B = 12, m_ratio = 0.1, seed = 1)
  expect_gte(fit$B, 3)
  expect_error(
    cbe_loco_mp_coxnet(x = as.matrix(df[3:5])[1:3, ], y = survival::Surv(df$time[1:3], df$status[1:3])),
    "at least 4 subjects"
  )
})

test_that("loco_subject_deltas matches a naive per-subject loop over patches", {
  set.seed(8)
  n <- 12
  n_eval <- 4
  n_patch <- 9
  eval_time <- c(1, 2, 4, 7)
  delta_t <- diff(eval_time)
  time_range <- diff(range(eval_time))
  weights <- matrix(stats::runif(n * n_eval, 0, 2), n, n_eval)
  alive <- matrix(stats::rbinom(n * n_eval, 1, 0.5), n, n_eval)
  oob <- matrix(stats::rbinom(n * n_patch, 1, 0.4), n, n_patch)
  surv <- array(stats::runif(n * n_eval * n_patch), c(n, n_eval, n_patch))
  for (k in seq_len(n_patch)) surv[oob[, k] == 0, , k] <- 0  # in the bag: no prediction
  with <- c(1, 1, 0, 1, 0, 0, 1, 0, 1)
  without <- 1 - with

  got <- loco_subject_deltas(with, without, matrix(surv, n * n_eval, n_patch), oob, weights, alive, delta_t, time_range)

  naive <- numeric(0)
  for (i in seq_len(n)) {
    use_with <- which(with == 1 & oob[i, ] == 1)
    use_without <- which(without == 1 & oob[i, ] == 1)
    if (!length(use_with) || !length(use_without)) next
    mean_curve <- function(ks) rowMeans(vapply(ks, function(k) surv[i, , k], numeric(n_eval)))
    loss <- function(s) weights[i, ] * (alive[i, ] - s)^2
    trap <- function(l) sum(delta_t * (l[-1] + l[-n_eval]) / 2) / time_range
    naive <- c(naive, trap(loss(mean_curve(use_without))) - trap(loss(mean_curve(use_with))))
  }
  expect_gt(length(naive), 3)
  expect_equal(got, naive)
})
