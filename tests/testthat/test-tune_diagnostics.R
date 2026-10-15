test_that("fit_ibs_mixed_model works with valid tuning data", {
  df <- data.frame(
    inner_resamples_splits = rep(c("Split1", "Split2", "Split3"), each = 4),
    alpha = rep(c(0.1, 0.5, 0.8, 1.0), times = 3),
    lambda = 0.05,
    IBS = c(0.12, 0.14, 0.16, 0.18, 0.11, 0.13, 0.15, 0.17, 0.13, 0.15, 0.17, 0.19),
    term = "age",
    estimate = 0.2
  )

  # Model with log_ibs = TRUE
  res <- fit_ibs_mixed_model(df, log_ibs = TRUE)
  expect_s3_class(res, "cbe_tune_mixed_model")
  expect_true(is.character(res$equation))
  expect_true(grepl("log10\\(IBS\\)", res$equation))
  expect_true(is.data.frame(res$coefficients))
  expect_true(is.data.frame(res$fitted))
  expect_equal(nrow(res$fitted), 100L)

  # Model with log_ibs = FALSE
  res_raw <- fit_ibs_mixed_model(df, log_ibs = FALSE)
  expect_true(grepl("^IBS =", res_raw$equation))

  # Errors on missing columns or empty data
  expect_error(fit_ibs_mixed_model(data.frame()), "non-empty data frame")
  expect_error(fit_ibs_mixed_model(data.frame(x = 1)), "missing required column")
})

test_that("plot_tune_alpha_ibs creates expected ggplot", {
  df <- data.frame(
    inner_resamples_splits = rep(c("Split1", "Split2"), each = 2),
    alpha = rep(c(0.2, 0.8), times = 2),
    IBS = c(0.15, 0.19, 0.14, 0.18)
  )

  p <- plot_tune_alpha_ibs(df)
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$labels$subtitle))
  expect_true(grepl("log10\\(IBS\\)", p$labels$subtitle))
})

test_that("plot_tune_lambda_ibs and plot_tune_ibs_by_resample work", {
  df <- data.frame(
    inner_resamples_splits = rep(c("Split1", "Split2"), each = 3),
    alpha = rep(c(0.1, 0.5, 1.0), times = 2),
    lambda = rep(c(0.01, 0.05, 0.1), times = 2),
    IBS = c(0.15, 0.14, 0.16, 0.16, 0.15, 0.17)
  )

  p_lambda <- plot_tune_lambda_ibs(df)
  expect_s3_class(p_lambda, "ggplot")

  p_resample <- plot_tune_ibs_by_resample(df)
  expect_s3_class(p_resample, "ggplot")

  expect_error(plot_tune_lambda_ibs(df[, c("alpha", "IBS")]), "must contain 'lambda'")
  expect_error(plot_tune_ibs_by_resample(df[, c("alpha", "IBS")]), "must contain a split identifier")
})

test_that("plot_tune_coef_boxplot works with feature terms", {
  df <- data.frame(
    term = c("(Intercept)", "age", "bmi", "sex", "(Intercept)", "age", "bmi", "sex"),
    estimate = c(0.1, 0.5, 0.2, -0.1, 0.1, 0.45, 0.25, -0.05),
    inner_resamples_splits = rep(c("Split1", "Split2"), each = 4)
  )

  p <- plot_tune_coef_boxplot(df)
  expect_s3_class(p, "ggplot")
  # Intercept should be excluded
  expect_false("(Intercept)" %in% levels(p$data$term))

  p_top <- plot_tune_coef_boxplot(df, top_n = 2)
  expect_s3_class(p_top, "ggplot")
  expect_equal(nlevels(p_top$data$term), 2L)

  expect_error(plot_tune_coef_boxplot(data.frame(x = 1)), "must contain 'term' and 'estimate'")
})

test_that("tbl_tune_alpha and tbl_tune_coef return tidy summary tables", {
  df <- data.frame(
    inner_resamples_splits = rep(c("Split1", "Split2"), each = 4),
    alpha = rep(c(0.2, 0.8), each = 2, times = 2),
    lambda = 0.01,
    IBS = rep(c(0.12, 0.18), each = 2, times = 2),
    term = rep(c("age", "bmi"), times = 4),
    estimate = c(0.4, 0.0, 0.5, 0.1, 0.38, 0.0, 0.52, 0.08)
  )

  # Alpha table
  tbl_a <- tbl_tune_alpha(df)
  expect_s3_class(tbl_a, "tbl_df")
  expect_equal(nrow(tbl_a), 2L)
  expect_true(all(c("alpha", "mean_ibs", "median_ibs") %in% names(tbl_a)))

  # Coef table
  tbl_c <- tbl_tune_coef(df)
  expect_s3_class(tbl_c, "tbl_df")
  expect_equal(nrow(tbl_c), 2L)
  expect_true(all(c("term", "prop_selected", "mean_estimate", "median_estimate") %in% names(tbl_c)))
  expect_equal(tbl_c$n_selected[tbl_c$term == "age"], 4L)
})
