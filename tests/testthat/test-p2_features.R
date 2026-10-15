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

test_that("tune_best_glmnet_workflow tunes and finalizes a penalized Cox workflow", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  set.seed(123)
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])

  res <- suppressWarnings(
    tune_best_glmnet_workflow(
      survival::Surv(time, status) ~ age + sex + ph.ecog,
      data = lung,
      mixture = c(0.5, 1.0),
      v = 3
    )
  )

  expect_type(res, "list")
  expect_named(res, c("best_mixture", "best_penalty", "model", "cv_results"))
  expect_true(res$best_mixture %in% c(0.5, 1.0))
  expect_true(is.numeric(res$best_penalty) && res$best_penalty > 0)
  expect_s3_class(res$model, "coxnet_model")
  expect_s3_class(res$cv_results, "cv_coxnet")
})

test_that("write_run_manifest records metadata JSON beside outputs", {
  skip_if_not_installed("jsonlite")

  tmp_dir <- tempfile("manifest_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  dummy_results <- data.frame(
    file = c("report1.qmd", "report2.qmd"),
    output = c("pdf", "docx"),
    duration = c(12.3, 4.5),
    status = c("success", "success"),
    stringsAsFactors = FALSE
  )

  mf_path <- write_run_manifest(tmp_dir, dummy_results, seed = 2026)
  expect_true(file.exists(mf_path))

  data <- jsonlite::read_json(mf_path)
  expect_true("timestamp" %in% names(data))
  expect_true("templecbe_version" %in% names(data))
  expect_true("future_plan" %in% names(data))
  expect_equal(data$seed, 2026)
  expect_true("package_versions" %in% names(data))
  expect_true("per_node_wall_time" %in% names(data))
  expect_equal(length(data$per_node_wall_time), 2)
})

test_that("zip_reports fails loudly when docx_strict = TRUE and no backend exists", {
  skip_if_not_installed("openxlsx")

  tmp_dir <- tempfile("zip_strict_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy report PDF without a DOCX
  pdf_file <- file.path(tmp_dir, "test_rep.pdf")
  writeLines("%PDF-1.4 dummy", pdf_file)

  rep_df <- data.frame(
    name = "Strict Report",
    path = sub("\\.pdf$", ".qmd", pdf_file),
    stringsAsFactors = FALSE
  )

  # When docx_strict = TRUE, fails if no backend exists or converter returns FALSE
  expect_error(
    zip_reports(
      rep_df,
      output_formats = c("pdf", "docx"),
      output_dir = tmp_dir,
      docx_from_pdf = function(src, dest) FALSE,
      docx_strict = TRUE
    ),
    "docx_strict = TRUE"
  )
})

test_that("render skip_downstream marks dependent nodes as skipped_upstream_failure", {
  tmp_dir <- tempfile("render_skip_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create two dummy scripts/reports
  qmd1 <- file.path(tmp_dir, "step1.qmd")
  qmd2 <- file.path(tmp_dir, "step2.qmd")
  writeLines(c("---", "title: Step 1", "---", "", "```{r}", "stop('Intentional failure in step 1')", "```"), qmd1)
  writeLines("title: Step 2", qmd2)

  node1 <- FileOutputs(name = "step1", path = qmd1, renders = TRUE)
  node2 <- FileOutputs(name = "step2", path = qmd2, renders = TRUE, dependencies = list(FilePath(name = "step1", path = qmd1)))

  pipe <- list(node1, node2)

  # Rerun shorthand works
  plan_dry <- render(pipe, rerun = "full", dry_run = TRUE, renv_guard = FALSE)
  expect_equal(nrow(plan_dry), 2)

  # We verify render with on_error = "continue", rerun = "full", and skip_downstream = TRUE
  res <- suppressWarnings(
    render(pipe, rerun = "full", formats = "pdf", on_error = "continue", skip_downstream = TRUE, manifest = FALSE, renv_guard = FALSE)
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$file[1], "step1.qmd")
  expect_true(res$status[1] != "success")
  expect_equal(res$file[2], "step2.qmd")
  expect_equal(res$status[2], "skipped_upstream_failure")
  expect_equal(res$duration[2], 0)
})

test_that("render parallel wave execution renders independent branches even when another branch fails", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile("render_wave_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Branch A: a1 (fails) -> a2 (depends on a1)
  # Branch B: b1 (succeeds) -> b2 (depends on b1)
  a1_rmd <- file.path(tmp_dir, "a1.Rmd")
  a2_rmd <- file.path(tmp_dir, "a2.Rmd")
  b1_rmd <- file.path(tmp_dir, "b1.Rmd")
  b2_rmd <- file.path(tmp_dir, "b2.Rmd")

  writeLines(c("---", "title: A1", "---", "```{r}", "stop('A1 error')", "```"), a1_rmd)
  writeLines(c("---", "title: A2", "---", "Hello from A2"), a2_rmd)
  writeLines(c("---", "title: B1", "---", "Hello from B1"), b1_rmd)
  writeLines(c("---", "title: B2", "---", "Hello from B2"), b2_rmd)

  node_a1 <- FileOutputs(name = "a1", path = a1_rmd, renders = TRUE)
  node_a2 <- FileOutputs(name = "a2", path = a2_rmd, renders = TRUE, dependencies = list(FilePath(name = "a1", path = a1_rmd)))
  node_b1 <- FileOutputs(name = "b1", path = b1_rmd, renders = TRUE)
  node_b2 <- FileOutputs(name = "b2", path = b2_rmd, renders = TRUE, dependencies = list(FilePath(name = "b1", path = b1_rmd)))

  pipe <- list(node_a1, node_a2, node_b1, node_b2)

  res <- suppressWarnings(
    render(pipe, rerun = "full", formats = "html", on_error = "continue", skip_downstream = TRUE,
           manifest = FALSE, renv_guard = FALSE, engine = "rmarkdown")
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)

  # a1 failed
  a1_row <- res[res$file == "a1.Rmd", ]
  expect_true(a1_row$status != "success")

  # a2 skipped due to upstream failure in branch A
  a2_row <- res[res$file == "a2.Rmd", ]
  expect_equal(a2_row$status, "skipped_upstream_failure")

  # b1 succeeded in branch B
  b1_row <- res[res$file == "b1.Rmd", ]
  expect_equal(b1_row$status, "success")

  # b2 succeeded in branch B because its upstream dependency b1 succeeded
  b2_row <- res[res$file == "b2.Rmd", ]
  expect_equal(b2_row$status, "success")
})

test_that("glmnet_IBS supports parallel options and does not clash with tune_method in dots", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("rsample")

  set.seed(42)
  long <- do.call(rbind, lapply(1:20, function(i) {
    k <- sample(1:2, 1)
    stops <- c(5, 10)[1:k]
    risk <- rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, head(stops, -1)),
      tstop = stops,
      status = c(rep(0, k - 1), rbinom(1, 1, 0.5)),
      x1 = rnorm(k),
      x2 = rnorm(k)
    )
  }))

  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable") |>
    recipes::step_range(recipes::all_numeric_predictors())
  split <- rsample::group_initial_split(long, group = subject_id)

  # Passing parallel = FALSE and parallel_over = "everything" works without warnings
  res <- glmnet_IBS(
    split,
    alpha = 0.5,
    recipe = rec,
    feature_names = c("x1", "x2"),
    internal_folds = 2,
    id_col = "subject_id",
    cox.ties = "breslow",
    parallel = FALSE,
    parallel_over = "everything"
  )
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("IBS", "lambda", "term", "estimate", "alpha") %in% names(res)))

  # tune_method passed in dots is not treated as tune_engine and does not cause error
  res_dots <- glmnet_IBS(
    split,
    alpha = 0.5,
    recipe = rec,
    feature_names = c("x1", "x2"),
    internal_folds = 2,
    id_col = "subject_id",
    cox.ties = "breslow",
    tune_method = "resample"
  )
  expect_s3_class(res_dots, "tbl_df")
})

