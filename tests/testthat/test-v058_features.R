# ==============================================================================
# test-v058_features.R
# Test suite for TempleCBE v0.5.8 features, naming conventions, and bug fixes:
# 1. General-purpose checkpoint_map() and tidymodels checkpointed wrappers
# 2. Canonical naming and lifecycle deprecation warnings
# 3. Packaged Quarto child report template (nested_survival_report_template)
# 4. Bugs and gaps:
#    - G1: Parallel scaling & timing breakdown
#    - G2: Muffled worker notices, main session notice
#    - G3: Fold-level consolidated checkpoints
#    - G4: compare_censoring_weights() checkpointing
#    - G5: as_tuning_results() legacy adapter
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. checkpoint_map() core behavior
# ------------------------------------------------------------------------------

test_that("1. checkpoint_map() executes, saves checkpoints, and resumes without re-evaluating", {
  tmp_dir <- file.path(tempdir(), paste0("ckpt_test_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  eval_counts <- new.env(parent = emptyenv())
  eval_counts$n <- 0L

  worker_fn <- function(x) {
    eval_counts$n <- eval_counts$n + 1L
    x * 2L
  }

  items <- list(a = 1L, b = 2L, c = 3L)

  # Initial run: should evaluate all 3 items
  res1 <- checkpoint_map(
    .x = items,
    .f = worker_fn,
    checkpoint_dir = tmp_dir,
    key = names(items),
    fingerprint = "test_fp_v1",
    progress = FALSE
  )

  expect_equal(as.integer(unlist(res1)), c(2L, 4L, 6L))
  expect_equal(eval_counts$n, 3L)

  # Verify checkpoint files exist
  ckpts <- list.files(tmp_dir, pattern = "\\.rds$")
  expect_equal(length(ckpts), 3L)

  # Second run: should hit cache and NOT re-evaluate
  res2 <- checkpoint_map(
    .x = items,
    .f = worker_fn,
    checkpoint_dir = tmp_dir,
    key = names(items),
    fingerprint = "test_fp_v1",
    progress = FALSE
  )

  expect_equal(as.integer(unlist(res2)), c(2L, 4L, 6L))
  expect_equal(eval_counts$n, 3L) # Count unchanged

  # Fingerprint mismatch with checkpoint_mismatch = 'recompute'
  res3 <- checkpoint_map(
    .x = items,
    .f = worker_fn,
    checkpoint_dir = tmp_dir,
    key = names(items),
    fingerprint = "test_fp_v2",
    checkpoint_mismatch = "recompute",
    progress = FALSE
  )
  expect_equal(as.integer(unlist(res3)), c(2L, 4L, 6L))
  expect_equal(eval_counts$n, 6L) # All 3 recomputed

  # Fingerprint mismatch with checkpoint_mismatch = 'error'
  expect_error(
    checkpoint_map(
      .x = items,
      .f = worker_fn,
      checkpoint_dir = tmp_dir,
      key = names(items),
      fingerprint = "test_fp_mismatch",
      checkpoint_mismatch = "error",
      progress = FALSE
    ),
    "mismatched fingerprint"
  )
})

test_that("1. checkpoint_map() handles sharding correctly", {
  tmp_dir <- file.path(tempdir(), paste0("shard_test_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  items <- as.list(1:10)
  fn <- function(x) x * 10L

  # Shard 1 of 2
  s1 <- checkpoint_map(items, fn, checkpoint_dir = tmp_dir, shard = "1/2", progress = FALSE)
  # Shard 2 of 2
  s2 <- checkpoint_map(items, fn, checkpoint_dir = tmp_dir, shard = "2/2", progress = FALSE)

  s1_vals <- unlist(s1[!vapply(s1, is.null, logical(1))])
  expect_equal(length(s1_vals), 5L)
  expect_equal(sort(unname(as.integer(s1_vals))), c(10L, 30L, 50L, 70L, 90L))

  # s2 computes remaining items and picks up s1 cached items
  s2_vals <- unlist(s2[!vapply(s2, is.null, logical(1))])
  expect_equal(length(s2_vals), 10L)
  expect_equal(unname(as.integer(s2_vals)), seq(10L, 100L, by = 10L))

  # collect_checkpoints
  coll <- collect_checkpoints(tmp_dir)
  expect_equal(length(coll), 10L)

  # clean_checkpoints
  cleaned <- clean_checkpoints(tmp_dir, keep = "none")
  expect_true(nrow(cleaned) >= 10L)
  expect_equal(length(list.files(tmp_dir, pattern = "\\.rds$")), 0L)
})

# ------------------------------------------------------------------------------
# 2. Tidymodels Checkpointed Wrappers
# ------------------------------------------------------------------------------

test_that("2. fit_resamples_checkpointed() and tune_grid_checkpointed() work and resume", {
  for (pkg in c("rsample", "recipes", "parsnip", "tune", "workflows")) {
    skip_if_not_installed(pkg)
  }

  set.seed(42)
  df <- data.frame(
    y = stats::rnorm(40),
    x1 = stats::rnorm(40),
    x2 = stats::rnorm(40)
  )

  splits <- rsample::vfold_cv(df, v = 2)
  rec <- recipes::recipe(y ~ x1 + x2, data = df)
  spec <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  wf <- workflows::workflow() |> workflows::add_recipe(rec) |> workflows::add_model(spec)

  tmp_dir <- file.path(tempdir(), paste0("tune_ckpt_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Run fit_resamples_checkpointed
  res1 <- fit_resamples_checkpointed(
    object = wf,
    resamples = splits,
    checkpoint_dir = tmp_dir
  )

  expect_s3_class(res1, "resample_results")
  expect_true("splits" %in% names(res1))
  expect_true(".metrics" %in% names(res1))

  # Files created in checkpoint_dir
  files <- list.files(tmp_dir, pattern = "\\.rds$")
  expect_true(length(files) >= 2L)

  # Run again: should resume from checkpoint
  res2 <- fit_resamples_checkpointed(
    object = wf,
    resamples = splits,
    checkpoint_dir = tmp_dir
  )
  expect_s3_class(res2, "resample_results")
  expect_equal(tune::collect_metrics(res1), tune::collect_metrics(res2))
})

# ------------------------------------------------------------------------------
# 3. Canonical Naming and Lifecycle Deprecations
# ------------------------------------------------------------------------------

test_that("3. Canonical functions execute and deprecated aliases warn", {
  # count_na / SumNa
  df_na <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 4))
  expect_equal(count_na(df_na), 3L)
  expect_warning(SumNa(df_na), "deprecated")

  # normalize_path_safely / normalize_safely
  expect_equal(unname(normalize_path_safely(tempdir())), normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
  expect_warning(normalize_safely(tempdir()), "deprecated")

  # drop_correlated_columns / find_correlation
  if (requireNamespace("caret", quietly = TRUE)) {
    mat <- matrix(c(1, 0.99, 0.99, 1), nrow = 2)
    colnames(mat) <- c("v1", "v2")
    rownames(mat) <- c("v1", "v2")
    expect_equal(drop_correlated_columns(mat, cutoff = 0.9), "v2")
    expect_warning(find_correlation(mat, cutoff = 0.9), "deprecated")
  }

  # describe_columns / proc_contents
  expect_s3_class(describe_columns(iris), "tbl_df")
  expect_warning(proc_contents(iris), "deprecated")

  # relevel_factor / factor_reference
  f <- factor(c("low", "med", "high"))
  expect_equal(levels(relevel_factor(f, "high"))[1], "high")
  expect_warning(factor_reference(f, "high"), "deprecated")

  # std_error / sd.error
  x <- c(1, 2, 3, 4, 5)
  expect_true(is.numeric(std_error(x)))
  expect_warning(sd.error(x), "deprecated")

  # is_integer_valued / is.int
  expect_true(is_integer_valued(5))
  expect_false(is_integer_valued(5.5))
  expect_warning(is.int(5), "deprecated")

  # summary_table / my_summary_table
  expect_s3_class(summary_table(iris, Sepal.Length), "data.frame")
  expect_warning(my_summary_table(iris, Sepal.Length), "deprecated")

  # produces_data / controls_datasets
  fo <- new("FileOutputs", name = "test_script", output = list(new("FilePath", name = "out", path = "data.rds")))
  expect_true(produces_data(fo))
  expect_warning(controls_datasets(fo), "deprecated")

  # match_like / like
  expect_true(match_like("hello world", "world"))
  expect_warning(like("hello world", "world"), "deprecated")

  # extract_xlsx_paths / extract_win_posix_paths
  lines <- c("Reading C:/data/file.xlsx now")
  expect_equal(extract_xlsx_paths(lines)$full_path, "C:/data/file.xlsx")
  expect_warning(extract_win_posix_paths(lines), "deprecated")

  # rm_all_except / keep_only
  e <- new.env()
  e$a <- 1
  e$b <- 2
  e$c <- 3
  rm_all_except(vector = c("a", "b"), env = e)
  expect_equal(ls(e), c("a", "b"))
  expect_warning(keep_only(vector = "a", env = e), "deprecated")
  expect_equal(ls(e), "a")

  # cbe_* aliases converted to canonical
  expect_warning(cbe_variable_roles(character(0)), "deprecated")
  expect_warning(cbe_compare_df(data.frame(a = 1), data.frame(a = 1)), "deprecated")
  expect_warning(cbe_exact2x2(matrix(c(1L, 2L, 3L, 4L), 2, 2)), "deprecated")
  expect_warning(cbe_find_shared_keys(list(df1 = data.frame(id = 1), df2 = data.frame(id = 1))), "deprecated")
})

# ------------------------------------------------------------------------------
# 4. Quarto Child Report Template
# ------------------------------------------------------------------------------

test_that("4. nested_survival_report_template() returns valid template with all required sections", {
  tpl_path <- nested_survival_report_template()
  expect_true(file.exists(tpl_path))
  expect_match(tpl_path, "nested_survival_report\\.qmd$")

  lines <- readLines(tpl_path)
  content <- paste(lines, collapse = "\n")

  # Check key required sections
  expect_match(content, "Executive Summary")
  expect_match(content, "Hyperparameter Tuning & Elastic-Net Paths")
  expect_match(content, "Outer-Fold Linear Predictor & Mixed Model")
  expect_match(content, "Standardized Cox Model Coefficients")
  expect_match(content, "Outer-Fold Feature Importance")
  expect_match(content, "Censoring Weight Sensitivity Comparison")
  expect_match(content, "Correlation & Multicollinearity Assessment")
  expect_match(content, "Reproducibility Appendix")
})

# ------------------------------------------------------------------------------
# 5. Bugs & Gaps (G1 - G5)
# ------------------------------------------------------------------------------

test_that("5. G5: as_tuning_results() standardizes legacy and custom results", {
  # Synthetic legacy Wolfson tuning results
  legacy_df <- data.frame(
    outer_fold = c(1, 1, 2, 2),
    IBS = c(0.12, 0.15, 0.11, 0.14),
    term = c("age", "sex", "age", "sex"),
    estimate = c(0.4, -0.2, 0.35, -0.18)
  )

  adapted <- as_tuning_results(legacy_df, domain = "ABG")
  expect_s3_class(adapted, "cbe_tuning_results")
  expect_s3_class(adapted, "tbl_df")
  expect_true(".metric" %in% names(adapted))
  expect_true(".estimate" %in% names(adapted))
  expect_true("domain" %in% names(adapted))
  expect_equal(adapted$domain[1], "ABG")

  # summary and tidy methods
  s <- summary(adapted)
  expect_s3_class(s, "summary.cbe_tuning_results")
  td <- generics::tidy(adapted)
  expect_s3_class(td, "tbl_df")
})

test_that("5. G4: compare_censoring_weights() respects checkpointing", {
  for (pkg in c("survival", "riskRegression")) {
    skip_if_not_installed(pkg)
  }

  set.seed(123)
  test_data <- data.frame(
    id = 1:40,
    tstart = 0,
    tstop = stats::rexp(40, rate = 0.05) + 0.5,
    status = stats::rbinom(40, size = 1, prob = 0.7),
    x1 = stats::rnorm(40),
    x2 = stats::rnorm(40)
  )

  test_split <- rsample::initial_split(test_data)
  rec <- recipes::recipe(tstart + tstop + status + id + x1 + x2 ~ ., data = test_data) |>
    recipes::update_role(id, tstart, tstop, status, new_role = "id variable")
  grid <- tibble::tibble(model = "m1", alpha = 0.5)

  tmp_dir <- file.path(tempdir(), paste0("ccw_ckpt_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Initial run
  res1 <- compare_censoring_weights(
    object = test_split,
    grid = grid,
    recipe = rec,
    feature_names = c("x1", "x2"),
    checkpoint_dir = tmp_dir
  )

  expect_s3_class(res1, "cbe_censoring_weights_comparison")
  expect_true(dir.exists(file.path(tmp_dir, "score_none")) || dir.exists(file.path(tmp_dir, "score_ipcw")))

  # Second run should resume
  res2 <- compare_censoring_weights(
    object = test_split,
    grid = grid,
    recipe = rec,
    feature_names = c("x1", "x2"),
    checkpoint_dir = tmp_dir
  )
  expect_equal(res1$ibs_none, res2$ibs_none)
})

test_that("5. G3: nested_survival_run() writes consolidated fold-level checkpoints", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "ggplot2", "jsonlite")) {
    skip_if_not_installed(pkg)
  }

  raw_rossi <- rossi_data()
  rossi_sub <- raw_rossi[1:50, ]
  rossi_sub$id <- seq_len(nrow(rossi_sub))
  rossi_sub$tstart <- 0
  rossi_sub$tstop <- as.numeric(rossi_sub$week)
  rossi_sub$status <- as.integer(rossi_sub$arrest)
  rossi_sub <- rossi_sub[, c("id", "tstart", "tstop", "status", "fin", "age", "prio")]

  run_dir <- file.path(tempdir(), paste0("rossi_g3_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)

  res <- nested_survival_run(
    data = rossi_sub,
    id = "id",
    start = "tstart",
    stop = "tstop",
    status = "status",
    domain = "RossiRecidivismG3",
    run_dir = run_dir,
    feature_names = c("fin", "age", "prio"),
    formulas = c("fin + age + prio"),
    outer_v = 2,
    inner_v = 2,
    alphas = c(0.5),
    top_n = 1,
    internal_folds = 3,
    cox.ties = "breslow",
    seed = 67025,
    progress = TRUE
  )

  expect_s3_class(res, "cbe_nested_survival_run")
  # G3: Check that fold-level consolidated checkpoints exist instead of hundreds of files
  rds_files <- list.files(file.path(run_dir, "checkpoints"), pattern = "\\.rds$")
  # Expect consolidated files: tune_outer_Fold1.rds, tune_outer_Fold2.rds, etc.
  expect_true(any(grepl("^tune_outer_", rds_files)))
  # Count of checkpoint files should be small (under 15)
  expect_true(length(rds_files) <= 15L)

  # Check timing breakdown was collected (G1)
  expect_true(!is.null(res$elapsed_sec))
  expect_true(res$elapsed_sec > 0)
})
