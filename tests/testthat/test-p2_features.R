test_that("correlated_pairs_table, plot_corr_scatter, plot_rank_heatmap work as expected", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    b = c(2, 4, 5, 8, 10, 12, 14, 16),
    c = c(8, 7, 6, 5, 4, 3, 2, 1),
    d = c(1, 5, 2, 8, 3, 7, 4, 6)
  )

  # correlated_pairs_table
  tbl <- correlated_pairs_table(df, cutoff = 0.8)
  expect_s3_class(tbl, "tbl_df")
  expect_true(nrow(tbl) >= 2)
  expect_true(all(c("var1", "var2", "r", "abs_r", "p_value") %in% names(tbl)))
  expect_true(all(tbl$abs_r >= 0.8))

  # plot_corr_scatter
  p_scat <- plot_corr_scatter(df, x = "a", y = "b")
  expect_s3_class(p_scat, "ggplot")
  expect_true(!is.null(p_scat$labels$subtitle))
  expect_true(grepl("r = ", p_scat$labels$subtitle))

  # plot_rank_heatmap
  rank_df <- data.frame(
    feature = rep(c("x1", "x2", "x3"), each = 3),
    fold = rep(c("Fold1", "Fold2", "Fold3"), times = 3),
    rank = c(1, 1, 2, 2, 3, 1, 3, 2, 3)
  )
  p_heat <- plot_rank_heatmap(rank_df, item_col = "feature", group_col = "fold", rank_col = "rank")
  expect_s3_class(p_heat, "ggplot")
})

test_that("label helpers: pretty_name, excel_colnames, copy_var_labels work", {
  # pretty_name
  names_raw <- c("patient_bmi", "follow_up_time", "serum_dna_ratio", "cardiac_hr")
  res_pretty <- pretty_name(names_raw)
  expect_equal(res_pretty[1], "Patient BMI")
  expect_equal(res_pretty[2], "Follow Up Time")
  expect_equal(res_pretty[3], "Serum DNA Ratio")
  expect_equal(res_pretty[4], "Cardiac HR")

  # excel_colnames
  excel_raw <- c("Patient/Subject [ID]", "Cost ($/hr)?", "Invalid*Name:Long")
  res_excel <- excel_colnames(excel_raw, max_char = 20)
  expect_false(any(grepl("[\\\\/?*:[\\]]", res_excel)))
  expect_true(all(nchar(res_excel) <= 20))

  # copy_var_labels
  d1 <- data.frame(age = 45, bmi = 23)
  attr(d1$age, "label") <- "Age at Baseline"
  attr(d1$bmi, "label") <- "Body Mass Index"
  d2 <- data.frame(age = 60, bmi = 27, sex = "M")
  d2 <- copy_var_labels(d1, d2)
  expect_equal(attr(d2$age, "label"), "Age at Baseline")
  expect_equal(attr(d2$bmi, "label"), "Body Mass Index")
  expect_null(attr(d2$sex, "label"))
})

test_that("fit_lmer_safely works and handles fallback", {
  df <- data.frame(
    id = factor(rep(1:4, each = 4)),
    x = rnorm(16),
    y = rnorm(16)
  )

  # With valid formula
  res <- fit_lmer_safely(y ~ x + (1 | id), data = df, warn_on_fallback = FALSE)
  expect_s3_class(res, "cbe_lmer_safely")
  expect_true(res$method %in% c("lmer", "lm"))
  expect_true(!is.null(res$model))

  # When formula has no random effects
  res_lm <- fit_lmer_safely(y ~ x, data = df, warn_on_fallback = FALSE)
  expect_equal(res_lm$method, "lm")
})

test_that("composite_index: center, compute_composite_index, index_variable_table work", {
  v <- c(10, 20, 30)
  expect_equal(center(v), c(-10, 0, 10))

  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(10, 20, 30, 40, 50),
    c = c(5, 4, 3, 2, 1)
  )

  # Z-score method
  df_z <- compute_composite_index(df, cols = c("a", "b"), method = "zscore", index_name = "idx")
  expect_true("idx" %in% names(df_z))
  expect_equal(mean(df_z$idx), 0)

  # PCA method
  df_pca <- compute_composite_index(df, cols = c("a", "b"), method = "pca", index_name = "idx_pca")
  expect_true("idx_pca" %in% names(df_pca))

  # Index variable table
  tbl <- index_variable_table(df_z, cols = c("a", "b"), index_col = "idx")
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2L)
  expect_true(all(tbl$r_with_index > 0.99))
})

test_that("km_cutpoints: km_cutpoint_search and plot_km_cutpoints work", {
  skip_if_not_installed("survival")
  set.seed(42)
  n <- 80
  marker <- rnorm(n, 50, 10)
  time <- rexp(n, 0.05) + ifelse(marker > 50, 5, 0)
  status <- rbinom(n, 1, 0.8)

  df <- data.frame(time = time, status = status, marker = marker)

  res <- km_cutpoint_search(df, "time", "status", "marker", min_prop = 0.2, n_cuts = 15)
  expect_s3_class(res, "cbe_km_cutpoint")
  expect_true(is.numeric(res$optimal_cutpoint))
  expect_true(is.data.frame(res$candidates))
  expect_true("marker_strata" %in% names(res$data))

  p <- plot_km_cutpoints(res)
  expect_s3_class(p, "ggplot")

  # Time-varying variant
  df_tv <- data.frame(
    id = rep(1:40, each = 2),
    tstart = rep(c(0, 5), times = 40),
    tstop = rep(c(5, 10), times = 40),
    status = rep(c(0, 1), times = 40),
    marker = rnorm(80, 50, 10)
  )
  res_tv <- km_cutpoint_time_varying(df_tv, "tstart", "tstop", "status", "marker", "id", min_prop = 0.2, n_cuts = 10)
  expect_s3_class(res_tv, "cbe_km_cutpoint")
})

test_that("imputation_diagnostics tools work", {
  set.seed(99)
  orig <- data.frame(x = c(1, 2, NA, 4, NA, 6, 7, 8, 9, 10), y = c(2, NA, 6, 8, 10, 12, 14, 16, 18, 20))
  imp <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), y = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))

  # compare_imputed_ecdf
  p_ecdf <- compare_imputed_ecdf(orig, imp, cols = c("x", "y"))
  expect_s3_class(p_ecdf, "ggplot")

  # test_imputed_distributions
  tests <- test_imputed_distributions(orig, imp, cols = c("x", "y"))
  expect_s3_class(tests, "tbl_df")
  expect_equal(nrow(tests), 2L)
  expect_equal(tests$n_imputed[tests$variable == "x"], 2L)

  # tune_impute_numeric
  complete_data <- data.frame(
    v1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    v2 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
  )
  tune_res <- tune_impute_numeric(complete_data, target_cols = c("v1", "v2"), methods = c("mean", "median"), prop_mask = 0.2)
  expect_s3_class(tune_res, "tbl_df")
  expect_true(all(c("variable", "method", "rse", "mae") %in% names(tune_res)))
})

test_that("workflow_tuning: summarize_dalex_importance and plots work", {
  fake_model <- list(coefficients = c("(Intercept)" = 0.5, age = 0.45, bmi = -0.32, bp = 0.12))
  imp <- summarize_dalex_importance(fake_model)
  expect_s3_class(imp, "tbl_df")
  expect_equal(nrow(imp), 3L)
  expect_equal(imp$variable[1], "age")

  p_imp <- plot_permutation_importance(imp)
  expect_s3_class(p_imp, "ggplot")

  tbl_imp <- tbl_permutation_importance(imp, top_n = 2)
  expect_equal(nrow(tbl_imp), 2L)
})

test_that("plot_deck extensions: plot_survival_km and plot_dynamic_trajectory", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")

  df <- data.frame(
    time = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = c(1, 0, 1, 1, 0, 1, 0, 1),
    arm = factor(rep(c("Control", "Treated"), each = 4))
  )

  # plot_survival_km with p_value and risk_table
  p_km <- plot_survival_km(df, "time", "status", group_col = "arm", risk_table = FALSE, p_value = TRUE)
  expect_s3_class(p_km, "ggplot")

  # plot_dynamic_trajectory with custom unnamed color vector
  long_df <- data.frame(
    time = rep(1:3, 4),
    val = rnorm(12),
    group = rep(c("Grp1", "Grp2"), each = 6)
  )
  p_dyn <- plot_dynamic_trajectory(long_df, "val", "time", "group", colors = c("#000000", "#FF0000"))
  expect_s3_class(p_dyn, "ggplot")
})
