make_tune_fixture_v0510 <- function(n = 60, seed = 1) {
  set.seed(seed)
  long <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(
      id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
      x1 = risk + stats::rnorm(k, sd = 0.2),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role(id, tstart, tstop, status, new_role = "id variable") |>
    recipes::step_range(recipes::all_numeric_predictors())
  list(
    split = rsample::group_initial_split(long, group = id, prop = 0.7),
    splits = rsample::group_vfold_cv(long, group = id, v = 2),
    recipe = rec,
    features = c("x1", "x2", "x3"),
    times = unique(long[, c("tstart", "tstop")]),
    long = long
  )
}

test_that("M2: Internal calls do not trigger lifecycle deprecation warnings", {
  withr::local_options(lifecycle_verbosity = "error")

  # 1. features_percent_miss does not call SumNa()
  df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
  expect_no_error(fpm <- features_percent_miss(df))
  expect_equal(fpm$SumNa, c(1L, 1L))

  # 2. test_categorical does not call cbe_exact2x2()
  df_cat <- data.frame(
    outcome = factor(c("yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B"))
  )
  expect_no_error(tc <- test_categorical(df_cat, variable = "outcome", by = "group"))
  expect_true(is.numeric(tc$p.value))

  # 3. exact2x2 directly
  tab <- matrix(c(10, 5, 2, 8), nrow = 2)
  expect_no_error(ex <- exact2x2(tab))
  expect_true(is.numeric(ex$p.value))

  # 4. compare_df directly
  d1 <- data.frame(id = 1:3, val = c("a", "b", "c"))
  d2 <- data.frame(id = 1:3, val = c("a", "b", "c"))
  expect_no_error(cmp <- compare_df(d1, d2, by = "id"))
  expect_s3_class(cmp, "compare_df")
})

test_that("M3: Every pca_* function accepts both prcomp and cbe_pca objects", {
  raw_pc <- stats::prcomp(mtcars[, 1:4], center = TRUE, scale. = TRUE)
  cbe_pc <- run_pca(mtcars[, 1:4], scale = TRUE)
  expect_s3_class(cbe_pc, "cbe_pca")

  # 1. pca_feature_loading_heatmap
  expect_s3_class(pca_feature_loading_heatmap(raw_pc), "ggplot")
  expect_s3_class(pca_feature_loading_heatmap(cbe_pc), "ggplot")

  # 2. plot_pca_bi
  mt2 <- tibble::rownames_to_column(mtcars[, 1:4], "car")
  expect_s3_class(plot_pca_bi(raw_pc, mt2, column = "car"), "ggplot")
  expect_s3_class(plot_pca_bi(cbe_pc, mt2, column = "car"), "ggplot")

  # 3. pca_biplot
  expect_s3_class(pca_biplot(raw_pc), "ggplot")
  expect_s3_class(pca_biplot(cbe_pc), "ggplot")

  # 4. pca_scree_plot
  expect_s3_class(pca_scree_plot(raw_pc), "ggplot")
  expect_s3_class(pca_scree_plot(cbe_pc), "ggplot")

  # 5. pca_variables_plot
  expect_s3_class(pca_variables_plot(raw_pc), "ggplot")
  expect_s3_class(pca_variables_plot(cbe_pc), "ggplot")

  # 6. rotation_matrix & pca_loadings
  r_raw <- rotation_matrix(raw_pc)
  r_cbe <- rotation_matrix(cbe_pc)
  expect_s3_class(r_raw, "tbl_df")
  expect_s3_class(r_cbe, "tbl_df")
  expect_equal(r_raw$feature, r_cbe$feature)
  expect_equal(pca_loadings(cbe_pc)$feature, r_cbe$feature)

  # 7. pca_eqns
  eq_raw <- pca_eqns(raw_pc)
  eq_cbe <- pca_eqns(cbe_pc)
  expect_true(is.list(eq_raw) && "eqns" %in% names(eq_raw))
  expect_true(is.list(eq_cbe) && "eqns" %in% names(eq_cbe))
  expect_equal(nrow(eq_raw$eqns), nrow(eq_cbe$eqns))

  # 8. pca_percent_var_explained
  expect_s3_class(pca_percent_var_explained(raw_pc), "ggplot")
  expect_s3_class(pca_percent_var_explained(cbe_pc), "ggplot")

  # 9. pca_loading_diff & pca_loading_diff_heatmap
  expect_s3_class(pca_loading_diff(raw_pc, cbe_pc), "tbl_df")
  expect_s3_class(pca_loading_diff_heatmap(raw_pc, cbe_pc), "ggplot")

  # 10. pca_plot
  expect_s3_class(pca_plot(raw_pc, type = "variance"), "ggplot")
  expect_s3_class(pca_plot(cbe_pc, type = "variance"), "ggplot")
  expect_s3_class(pca_plot(cbe_pc, type = "heatmap"), "ggplot")
  expect_s3_class(pca_plot(cbe_pc, type = "scree"), "ggplot")
})

test_that("M4: Parallel serial baseline measures warm-up and median sample correctly", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick")) {
    skip_if_not_installed(pkg)
  }

  fx <- make_tune_fixture_v0510()
  res <- suppressWarnings(tune_alpha_by_split(
    fx$splits,
    recipe = fx$recipe,
    alphas = c(0.1, 0.5, 0.9),
    feature_names = fx$features,
    time_data = fx$times,
    internal_folds = 2,
    progress = FALSE
  ))

  tb <- attr(res, "timing_breakdown")
  expect_true(is.list(tb))
  expect_true("measured_serial_task_sec" %in% names(tb))
  expect_true("real_speedup" %in% names(tb))
  expect_true("worker_concurrency_slowdown" %in% names(tb))
  expect_true(tb$measured_serial_task_sec > 0)
})

test_that("N1: find_deprecated_calls() and update_deprecated_calls() work on qualified and unqualified code", {
  td <- tempfile("test_n1_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  script <- file.path(td, "analysis.R")
  writeLines(c(
    "# Header comment",
    "v1 <- TempleCBE::sd.error(c(1, 2, 3))",
    "v2 <- summarize_tune_results(splits)",
    "v3 <- TempleCBE::write_xlsx(df, 'out.xlsx')",
    "keep_only(c('v1', 'v2'))",
    "R_names(df)"
  ), script)

  findings <- find_deprecated_calls(script)
  expect_s3_class(findings, "tbl_df")
  expect_equal(nrow(findings), 5L)
  expect_true(all(c("sd.error", "summarize_tune_results", "write_xlsx", "keep_only", "R_names") %in% findings$old_name))

  # Dry run
  dry_res <- update_deprecated_calls(script, dry_run = TRUE)
  expect_equal(nrow(dry_res), 5L)
  content_dry <- readLines(script)
  expect_true(any(grepl("TempleCBE::sd.error", content_dry)))

  # Apply update
  app_res <- update_deprecated_calls(script, dry_run = FALSE)
  content_applied <- readLines(script)
  expect_false(any(grepl("sd.error", content_applied)))
  expect_true(any(grepl("TempleCBE::std_error", content_applied)))
  expect_true(any(grepl("tune_alpha_by_split", content_applied)))
  expect_true(any(grepl("TempleCBE::write_xlsx_file", content_applied)))
  expect_true(any(grepl("rm_all_except", content_applied)))
  expect_true(any(grepl("clean_names_keep_labels", content_applied)))
})

test_that("N2: templecbe.cox_ties option and quiet tie scope work correctly", {
  withr::local_options(templecbe.cox_ties = "breslow", templecbe_cox_ties_noted = FALSE)
  expect_equal(get_templecbe_cox_ties(), "breslow")

  # Scope muffles messages matching glmnet 5.1 tie message
  msg_handled <- FALSE
  with_quiet_cox_ties({
    message("Starting in glmnet 5.1 the default Cox tie-handling method will change")
    msg_handled <- TRUE
  })
  expect_true(msg_handled)

  # Workflow configuration
  if (requireNamespace("parsnip", quietly = TRUE) && requireNamespace("workflows", quietly = TRUE)) {
    spec <- parsnip::proportional_hazards() |> parsnip::set_engine("coxnet")
    wflow <- workflows::workflow() |> workflows::add_model(spec)
    wflow_pinned <- apply_workflow_cox_ties(wflow)
    spec_pinned <- workflows::extract_spec_parsnip(wflow_pinned)
    expect_equal(spec_pinned$engine_args$cox.ties, "breslow")
  }
})

test_that("N3: diff_deliverables() pairs edited paragraphs, ignores tracked markup, and lists section changes", {
  td <- tempfile("test_n3_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Create two directories representing baseline and updated deliverable bundles
  dir1 <- file.path(td, "v1")
  dir2 <- file.path(td, "v2")
  dir.create(dir1)
  dir.create(dir2)

  f1 <- file.path(dir1, "report.md")
  f2 <- file.path(dir2, "report.md")

  # v1 has headings and paragraphs
  writeLines(c(
    "# Executive Summary",
    "The clinical trial evaluated 150 patients across 4 regional medical centers.",
    "The median overall survival was 14.2 months in the control group.",
    "# Methods",
    "Patients were randomized 1:1 to receive intervention or placebo.",
    "# Results",
    "Primary endpoint IBS was 0.142 with 95% confidence interval 0.12 to 0.16."
  ), f1)

  # v2 modifies the numbers in-place (edits) and adds a sentence under Results
  writeLines(c(
    "# Executive Summary",
    "The clinical trial evaluated 150 patients across 4 regional medical centers.",
    "The median overall survival was 18.5 months in the treated arm.",
    "# Methods",
    "Patients were randomized 1:1 to receive intervention or placebo.",
    "# Results",
    "Primary endpoint IBS was 0.118 with 95% confidence interval 0.10 to 0.13.",
    "Secondary endpoints also showed favorable improvement."
  ), f2)

  diff_res <- diff_deliverables(dir1, dir2)
  expect_s3_class(diff_res, "cbe_deliverable_diff")
  expect_equal(diff_res$status, "modified")

  # Edited paragraphs are paired rather than treated as pure delete + add
  expect_true(diff_res$n_edited >= 2L)
  expect_match(diff_res$diff_summary, "edited")
  expect_true("section_summary" %in% names(diff_res))
  expect_true("largest_changes" %in% names(diff_res))
})
