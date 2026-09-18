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
