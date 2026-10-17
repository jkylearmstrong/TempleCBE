# ==============================================================================
# Unit Tests for TempleCBE v0.5.6 Bug Fixes (C1-C3) & Features (N1-N9)
# ==============================================================================

make_v056_surv_data <- function(n_subj = 36, seed = 101) {
  withr::local_seed(seed)
  do.call(rbind, lapply(seq_len(n_subj), function(i) {
    k <- sample(1:3, 1)
    stops <- c(5, 10, 20)[seq_len(k)]
    x1 <- stats::rnorm(1)
    data.frame(
      id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * x1))),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

# ------------------------------------------------------------------------------
# C1: Checkpoint fingerprinting in score_outer() and summarize_tune_results()
# ------------------------------------------------------------------------------

test_that("C1: score_outer() invalidates checkpoints when censoring_weights, features, or cox.ties change", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 36, seed = 201)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  grid1 <- tibble::tibble(
    alpha = 0.5,
    formula = "x1 + x2 + x3"
  )

  ckpt_dir <- file.path(tempdir(), paste0("ckpt_c1_score_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_dir, recursive = TRUE), add = TRUE)

  # Initial run with IPCW
  res_ipcw <- score_outer(
    object = splits,
    grid = grid1,
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    censoring_weights = "ipcw",
    checkpoint_dir = ckpt_dir,
    seed = 42
  )

  # Changing censoring_weights from "ipcw" to "none" with same checkpoint_dir:
  # - With checkpoint_mismatch = "error", must stop
  expect_error(
    score_outer(
      object = splits,
      grid = grid1,
      recipe = rec,
      feature_names = c("x1", "x2", "x3"),
      time_data = times,
      internal_folds = 3,
      censoring_weights = "none",
      checkpoint_dir = ckpt_dir,
      checkpoint_mismatch = "error",
      seed = 42
    ),
    "mismatch"
  )

  # - With default checkpoint_mismatch = "recompute", must warn and recompute
  expect_warning(
    res_none <- score_outer(
      object = splits,
      grid = grid1,
      recipe = rec,
      feature_names = c("x1", "x2", "x3"),
      time_data = times,
      internal_folds = 3,
      censoring_weights = "none",
      checkpoint_dir = ckpt_dir,
      seed = 42
    ),
    "mismatched fingerprints"
  )

  # Verify against fresh run without checkpoints
  res_none_fresh <- suppressWarnings(score_outer(
    object = splits,
    grid = grid1,
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    censoring_weights = "none",
    checkpoint_dir = NULL,
    seed = 42
  ))
  expect_equal(res_none$IBS, res_none_fresh$IBS)

  # Changing feature set and cox.ties must also trigger recompute (not return stale cache)
  grid2 <- tibble::tibble(
    alpha = 0.5,
    formula = "x1 + x2"
  )
  expect_warning(
    res_reduced <- score_outer(
      object = splits,
      grid = grid2,
      recipe = rec,
      feature_names = c("x1", "x2"),
      time_data = times,
      internal_folds = 3,
      censoring_weights = "ipcw",
      cox.ties = "breslow",
      checkpoint_dir = ckpt_dir,
      seed = 42
    ),
    "mismatched fingerprints"
  )
  coefs_reduced <- tidy_outer_coefficients(res_reduced)
  expect_false("x3" %in% coefs_reduced$term)
})

test_that("C1: summarize_tune_results() invalidates checkpoints when feature set changes", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 36, seed = 202)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  ckpt_dir <- file.path(tempdir(), paste0("ckpt_c1_tune_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_dir, recursive = TRUE), add = TRUE)

  res_3feat <- summarize_tune_results(
    object = splits,
    alphas = c(0.2, 0.8),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    checkpoint_dir = ckpt_dir,
    seed = 99
  )
  expect_true("x3" %in% unique(res_3feat$term))

  # Rerunning with reduced features must error when checkpoint_mismatch = "error"
  expect_error(
    summarize_tune_results(
      object = splits,
      alphas = c(0.2, 0.8),
      recipe = rec,
      feature_names = c("x1", "x2"),
      time_data = times,
      internal_folds = 3,
      checkpoint_dir = ckpt_dir,
      checkpoint_mismatch = "error",
      seed = 99
    ),
    "mismatch"
  )

  # And warn + recompute by default, no longer returning x3
  expect_warning(
    res_2feat <- summarize_tune_results(
      object = splits,
      alphas = c(0.2, 0.8),
      recipe = rec,
      feature_names = c("x1", "x2"),
      time_data = times,
      internal_folds = 3,
      checkpoint_dir = ckpt_dir,
      seed = 99
    ),
    "mismatched fingerprints"
  )
  expect_false("x3" %in% unique(res_2feat$term))
})

# ------------------------------------------------------------------------------
# C2: All-failed tuning stops with an informative error
# ------------------------------------------------------------------------------

test_that("C2: summarize_tune_results() stops when features do not exist or all IBS are NA", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 30, seed = 203)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  expect_error(
    summarize_tune_results(
      object = splits,
      alphas = c(0.2, 0.8),
      recipe = rec,
      feature_names = c("nonexistent_feat_1", "nonexistent_feat_2"),
      time_data = times,
      internal_folds = 3,
      seed = 11
    ),
    "failed|None of the requested"
  )
})

# ------------------------------------------------------------------------------
# C3: glmnet_IBS() does not attach recipes to search()
# ------------------------------------------------------------------------------

test_that("C3: glmnet_IBS() does not leave package:recipes attached on search()", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 30, seed = 204)
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  was_attached <- "package:recipes" %in% search()
  if (was_attached) {
    try(detach("package:recipes", unload = FALSE, character.only = TRUE), silent = TRUE)
    on.exit({
      if (was_attached && !("package:recipes" %in% search())) {
        tryCatch(
          suppressPackageStartupMessages(attachNamespace("recipes")),
          error = function(e) try(suppressPackageStartupMessages(library(recipes)), silent = TRUE)
        )
      }
    }, add = TRUE)
  }

  # Create a recipe using an unqualified selector (all_numeric_predictors)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable") |>
    recipes::step_normalize(all_numeric_predictors())

  res <- glmnet_IBS(
    object = splits$splits[[1]],
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    alpha = 0.5,
    time_data = times,
    internal_folds = 3
  )

  expect_s3_class(res, "tbl_df")
  expect_false("package:recipes" %in% search())
})

# ------------------------------------------------------------------------------
# N1: Content-hash staleness for data and deliverables
# ------------------------------------------------------------------------------

test_that("N1: is_stale(stale_by = 'hash') and record_pipeline_hashes() detect content changes vs mtime touches", {
  tmp_dir <- file.path(tempdir(), paste0("hash_stale_", floor(stats::runif(1, 1e5, 9e5))))
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  in_file <- file.path(tmp_dir, "input.csv")
  out_file <- file.path(tmp_dir, "output.rds")
  script_file <- file.path(tmp_dir, "stage1.R")
  hash_file <- file.path(tmp_dir, "pipeline_hashes.json")

  writeLines("a,b\n1,2\n3,4", in_file)
  writeLines("# stage 1", script_file)
  saveRDS(list(ok = TRUE), out_file)

  node <- FileOutputs(
    name = "stage1_node",
    path = script_file,
    dependencies = list(FilePath("in_csv", in_file)),
    output = list(FilePath("out_rds", out_file)),
    renders = FALSE
  )

  # Before recording hashes, stale_by = "hash" treats unrecorded files as stale
  expect_true(is_stale(node, stale_by = "hash", hash_file = hash_file))

  # Record current hashes
  record_pipeline_hashes(list(node), hash_file = hash_file)
  expect_true(file.exists(hash_file))

  # Now node is fresh under hash mode
  expect_false(is_stale(node, stale_by = "hash", hash_file = hash_file))

  # Touching input mtime forward in time makes mtime mode stale, but hash mode stays fresh!
  Sys.setFileTime(in_file, Sys.time() + 3600)
  node_refreshed <- FileOutputs(
    name = "stage1_node",
    path = script_file,
    dependencies = list(FilePath("in_csv", in_file)),
    output = list(FilePath("out_rds", out_file)),
    renders = FALSE
  )
  expect_true(is_stale(node_refreshed, stale_by = "mtime"))
  expect_false(is_stale(node_refreshed, stale_by = "hash", hash_file = hash_file))

  # Modifying the actual content of in_file makes hash mode stale
  writeLines("a,b\n1,2\n3,999", in_file)
  expect_true(is_stale(node_refreshed, stale_by = "hash", hash_file = hash_file))
})

# ------------------------------------------------------------------------------
# N2: Multi-machine sharding & collect_checkpoints()
# ------------------------------------------------------------------------------

test_that("N2: shard = c(i, n) and collect_checkpoints() split and merge work across machines", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 36, seed = 205)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  # 1. Sharded summarize_tune_results
  ckpt_tune <- file.path(tempdir(), paste0("ckpt_n2_tune_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_tune, recursive = TRUE), add = TRUE)

  s1 <- summarize_tune_results(
    object = splits, alphas = c(0.2, 0.8),
    recipe = rec, feature_names = c("x1", "x2", "x3"),
    time_data = times, internal_folds = 3,
    checkpoint_dir = ckpt_tune, shard = c(1, 2), seed = 77
  )
  s2 <- summarize_tune_results(
    object = splits, alphas = c(0.2, 0.8),
    recipe = rec, feature_names = c("x1", "x2", "x3"),
    time_data = times, internal_folds = 3,
    checkpoint_dir = ckpt_tune, shard = c(2, 2), seed = 77
  )

  merged_tune <- collect_checkpoints(ckpt_tune, type = "tune")
  full_tune <- summarize_tune_results(
    object = splits, alphas = c(0.2, 0.8),
    recipe = rec, feature_names = c("x1", "x2", "x3"),
    time_data = times, internal_folds = 3,
    checkpoint_dir = NULL, seed = 77
  )
  expect_equal(merged_tune$IBS, full_tune$IBS)
  expect_equal(merged_tune$estimate, full_tune$estimate)

  # 2. Sharded score_outer
  ckpt_score <- file.path(tempdir(), paste0("ckpt_n2_score_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_score, recursive = TRUE), add = TRUE)

  grid <- tibble::tibble(
    alpha = c(0.2, 0.8),
    formula = c("x1 + x2", "x1 + x2 + x3")
  )

  score_outer(
    object = splits, grid = grid, recipe = rec,
    feature_names = c("x1", "x2", "x3"), time_data = times, internal_folds = 3,
    checkpoint_dir = ckpt_score, shard = c(1, 2), seed = 55
  )
  score_outer(
    object = splits, grid = grid, recipe = rec,
    feature_names = c("x1", "x2", "x3"), time_data = times, internal_folds = 3,
    checkpoint_dir = ckpt_score, shard = c(2, 2), seed = 55
  )

  merged_score <- collect_checkpoints(ckpt_score, type = "score_outer")
  expect_equal(nrow(merged_score), 2L * 2L)

  # Corrupting one checkpoint's run_fingerprint causes collect_checkpoints() to reject mixed runs
  rds_files <- list.files(ckpt_score, pattern = "^score_fold_.*\\.rds$", full.names = TRUE)
  bad_obj <- readRDS(rds_files[1])
  attr(bad_obj, "checkpoint_run_fingerprint") <- "mismatched_run_hash"
  saveRDS(bad_obj, rds_files[1])
  expect_error(
    collect_checkpoints(ckpt_score, type = "score_outer"),
    "Mixed runs|mismatch"
  )
})

# ------------------------------------------------------------------------------
# N3: Flat split x alpha parallelism in summarize_tune_results()
# ------------------------------------------------------------------------------

test_that("N3: summarize_tune_results() flat task execution is deterministic for the same seed", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v056_surv_data(n_subj = 30, seed = 206)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  run1 <- summarize_tune_results(
    object = splits, num_alpha_values = 4, num_fixed = 2,
    recipe = rec, feature_names = c("x1", "x2", "x3"),
    time_data = times, internal_folds = 3, seed = 1234
  )
  run2 <- summarize_tune_results(
    object = splits, num_alpha_values = 4, num_fixed = 2,
    recipe = rec, feature_names = c("x1", "x2", "x3"),
    time_data = times, internal_folds = 3, seed = 1234
  )

  # 2 splits * 4 alphas * 3 features = 24 rows
  expect_equal(nrow(run1), 2L * 4L * 3L)
  expect_equal(run1$IBS, run2$IBS)
  expect_equal(run1$alpha, run2$alpha)
  expect_equal(run1$lambda, run2$lambda)
  expect_equal(run1$estimate, run2$estimate)
})

# ------------------------------------------------------------------------------
# N4: validate_start_stop() pre-flight check for start/stop survival data
# ------------------------------------------------------------------------------

test_that("N4: validate_start_stop() reports anomalies and warns when there is no censoring", {
  # Cohort where every subject has an event (no censoring) + interval anomalies
  bad_df <- tibble::tibble(
    id = c(1, 1, 2, 2, 3, 3),
    start = c(0, 4, 0, 6, 0, 5),
    stop  = c(5, 8, 4, 9, 5, 5), # Subj 1 overlaps (5 > 4), Subj 2 has gap (4 < 6), Subj 3 has zero-length (5 == 5)
    status = c(1, 1, 0, 1, 1, 0) # Subj 1 & 3 have event before last interval & row after event; all 3 subjects have events!
  )

  expect_warning(
    val <- validate_start_stop(bad_df, id = "id", start = "start", stop = "stop", status = "status"),
    "No censoring detected"
  )

  expect_s3_class(val, "cbe_start_stop_validation")
  expect_false(val$valid)
  expect_equal(val$n_subjects, 3L)
  expect_equal(val$n_censored, 0L)
  expect_equal(val$n_overlapping, 1L)
  expect_equal(val$n_gaps, 1L)
  expect_equal(val$n_nonpositive_length, 1L)
  expect_true(val$n_events_before_last >= 1L)
  expect_true(val$n_rows_after_event >= 1L)

  # Clean cohort with censoring passes without warning
  clean_df <- tibble::tibble(
    id = c(1, 1, 2, 2),
    start = c(0, 5, 0, 4),
    stop = c(5, 10, 4, 8),
    status = c(0, 1, 0, 0)
  )
  expect_no_warning(
    val_clean <- validate_start_stop(clean_df, id = "id", start = "start", stop = "stop", status = "status")
  )
  expect_true(val_clean$valid)
})

# ------------------------------------------------------------------------------
# N5 & N6: Package-wide label map + outer-fold importance summary and plot
# ------------------------------------------------------------------------------

test_that("N5 & N6: cbe_labels(), tbl_label_map(), summarize_outer_importance(), and plot_outer_importance() work end-to-end", {
  on.exit(cbe_labels(reset = TRUE), add = TRUE)

  lmap <- tibble::tibble(
    term = c("x1", "x2", "x3"),
    short = c("PaO2/FiO2", "PEEP", "pH"),
    full = c("Arterial Oxygenation Ratio (PaO2/FiO2)", "Positive End-Expiratory Pressure (cmH2O)", "Arterial Blood pH")
  )

  cbe_labels(lmap, style = "short")
  expect_equal(
    resolve_cbe_labels(c("x1", "x2", "unknown_var")),
    c("PaO2/FiO2", "PEEP", "unknown_var")
  )
  expect_equal(
    resolve_cbe_labels(c("x1", "x2"), style = "full"),
    c("Arterial Oxygenation Ratio (PaO2/FiO2)", "Positive End-Expiratory Pressure (cmH2O)")
  )

  # Appendix table
  app_tbl <- tbl_label_map(format = "tibble")
  expect_s3_class(app_tbl, "tbl_df")
  expect_equal(nrow(app_tbl), 3L)

  # Outer importance summary & plot (N6) with label_map (N5)
  coef_long <- tibble::tibble(
    outer_fold = rep(c("Fold1", "Fold2", "Fold3"), each = 3),
    model = "M1",
    alpha = 0.5,
    lambda = 0.05,
    IBS = rep(c(0.12, 0.14, 0.11), each = 3),
    term = rep(c("x1", "x2", "x3"), times = 3),
    estimate = c(0.45, -0.20, 0.0, 0.50, -0.15, 0.0, 0.40, 0.0, 0.05)
  )

  imp <- summarize_outer_importance(coef_long)
  expect_s3_class(imp, "tbl_df")
  expect_true(all(c("term", "label", "selection_freq", "median_estimate", "min_estimate", "max_estimate") %in% names(imp)))
  # x1 is non-zero in all 3 folds
  x1_row <- imp[imp$term == "x1", ]
  expect_equal(x1_row$selection_freq, 1.0)
  expect_equal(x1_row$label, "PaO2/FiO2")

  p_imp <- plot_outer_importance(coef_long)
  expect_s3_class(p_imp, "ggplot")

  p_imp_freq <- plot_outer_importance(imp, type = "frequency")
  expect_s3_class(p_imp_freq, "ggplot")
})

# ------------------------------------------------------------------------------
# N7: estimate_runtime() pre-run benchmark
# ------------------------------------------------------------------------------

test_that("N7: estimate_runtime() times sample fits and projects total and per-machine runtime", {
  est <- estimate_runtime(
    fn = function(x) {
      Sys.sleep(0.01)
      x * 2
    },
    args = list(x = 5),
    n_sample = 2,
    n_outer_folds = 7,
    n_domains = 4,
    workers = 4,
    n_machines = 2
  )

  expect_s3_class(est, "cbe_runtime_estimate")
  expect_equal(est$n_sample, 2L)
  expect_equal(est$n_machines, 2L)
  expect_equal(est$workers, 4L)
  expect_true(est$projected_total_sec >= 0)
  expect_true(est$projected_per_machine_sec >= 0)
})

# ------------------------------------------------------------------------------
# N8: cbe_review_tracker() and validate_review_tracker()
# ------------------------------------------------------------------------------

test_that("N8: cbe_review_tracker() and validate_review_tracker() enforce location and rationale", {
  extracted <- list(
    comments = tibble::tibble(
      file = c("stage01.docx", "stage02.docx", "stage03.docx"),
      pipeline_stage = c("01_EDA", "02_Modeling", "03_Summary"),
      id = c("C1", "C2", "C3"),
      author = "PI",
      comment_text = c(
        "Please report interquartile ranges for skewed biomarkers in Table 2.",
        "Clarify why censoring weights were set to IPCW in the sensitivity section.",
        "ok"
      ),
      selected_text = c("Table 2", "Methods", "Intro")
    )
  )

  tracker <- cbe_review_tracker(extracted)
  expect_s3_class(tracker, "tbl_df")
  expect_equal(tracker$status, c("open", "open", "open"))
  expect_equal(tracker$trivial, c(FALSE, FALSE, TRUE))

  # Closing C1 with bare "done" or "TRUE" must fail validation
  bad_tracker <- tracker
  bad_tracker$status[1] <- "addressed"
  bad_tracker$location[1] <- "done"
  bad_tracker$rationale[1] <- "TRUE"

  expect_error(
    validate_review_tracker(bad_tracker, error_on_invalid = TRUE),
    "Review tracker validation failed"
  )

  val_bad <- validate_review_tracker(bad_tracker, error_on_invalid = FALSE)
  expect_false(val_bad$valid)
  expect_true(nrow(val_bad$issues) >= 1L)

  # Providing concrete location and rationale passes validation
  good_tracker <- tracker
  good_tracker$status[1] <- "addressed"
  good_tracker$location[1] <- "Section 3.2, Table 2 (page 7)"
  good_tracker$rationale[1] <- "Added median and IQR columns alongside mean (SD) for all skewed biomarkers."

  good_tracker$status[2] <- "not_adopted"
  good_tracker$location[2] <- "Section 2.4 (page 4)"
  good_tracker$rationale[2] <- "Retained IPCW as primary estimator because right-censoring exceeds 40%, and added unweighted sensitivity comparison in Appendix Table A2."

  good_tracker$status[3] <- "addressed"

  val_good <- validate_review_tracker(good_tracker, error_on_invalid = TRUE)
  expect_true(val_good$valid)
})

# ------------------------------------------------------------------------------
# N9: package_deliverables(stamp = TRUE) and diff_deliverables()
# ------------------------------------------------------------------------------

create_minimal_docx <- function(path, paragraphs) {
  tmp_d <- file.path(tempdir(), paste0("docx_build_", floor(stats::runif(1, 1e5, 9e5))))
  dir.create(file.path(tmp_d, "word"), recursive = TRUE)
  on.exit(unlink(tmp_d, recursive = TRUE), add = TRUE)

  p_nodes <- paste0(
    "<w:p><w:r><w:t>",
    vapply(paragraphs, function(p) {
      p <- gsub("&", "&amp;", p, fixed = TRUE)
      p <- gsub("<", "&lt;", p, fixed = TRUE)
      p <- gsub(">", "&gt;", p, fixed = TRUE)
      p
    }, character(1)),
    "</w:t></w:r></w:p>",
    collapse = ""
  )
  xml_str <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:body>", p_nodes, "</w:body></w:document>"
  )
  writeLines(xml_str, file.path(tmp_d, "word", "document.xml"))
  zip::zip(zipfile = path, files = "word", root = tmp_d, mode = "cherry-pick")
  invisible(path)
}

test_that("N9: package_deliverables(stamp = TRUE) date-stamps archive and diff_deliverables() diffs DOCX text", {
  skip_if_not_installed("zip")

  work_dir <- file.path(tempdir(), paste0("deliv_n9_", floor(stats::runif(1, 1e5, 9e5))))
  dir1 <- file.path(work_dir, "v1")
  dir2 <- file.path(work_dir, "v2")
  dir.create(dir1, recursive = TRUE)
  dir.create(dir2, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  create_minimal_docx(file.path(dir1, "report_a.docx"), c("Title A", "Old result: IBS = 0.142"))
  create_minimal_docx(file.path(dir1, "report_b.docx"), c("Title B", "Unchanged paragraph"))

  create_minimal_docx(file.path(dir2, "report_a.docx"), c("Title A", "New result: IBS = 0.118"))
  create_minimal_docx(file.path(dir2, "report_b.docx"), c("Title B", "Unchanged paragraph"))
  create_minimal_docx(file.path(dir2, "report_c.docx"), c("Title C", "Brand new report"))

  zip1 <- package_deliverables(
    data_deliverables = list(file.path(dir1, "report_a.docx"), file.path(dir1, "report_b.docx")),
    zip_path = file.path(work_dir, "bundle_v1.zip"),
    include_pipeline_graph = FALSE,
    stamp = "2026-10-01"
  )
  expect_match(basename(zip1), "bundle_v1_2026-10-01\\.zip$")
  expect_true(file.exists(zip1))

  zip2 <- package_deliverables(
    data_deliverables = list(
      file.path(dir2, "report_a.docx"),
      file.path(dir2, "report_b.docx"),
      file.path(dir2, "report_c.docx")
    ),
    zip_path = file.path(work_dir, "bundle_v2.zip"),
    include_pipeline_graph = FALSE,
    stamp = TRUE
  )
  expect_match(basename(zip2), paste0("bundle_v2_", format(Sys.Date(), "%Y-%m-%d"), "\\.zip$"))

  diff_res <- diff_deliverables(zip1, zip2)
  expect_s3_class(diff_res, "cbe_deliverable_diff")

  status_map <- stats::setNames(diff_res$status, basename(diff_res$file))
  expect_equal(unname(status_map["report_a.docx"]), "modified")
  expect_equal(unname(status_map["report_b.docx"]), "unchanged")
  expect_equal(unname(status_map["report_c.docx"]), "added")

  a_row <- diff_res[basename(diff_res$file) == "report_a.docx", ]
  expect_true(any(grepl("Old result: IBS = 0.142", a_row$diff_lines[[1]], fixed = TRUE)))
  expect_true(any(grepl("New result: IBS = 0.118", a_row$diff_lines[[1]], fixed = TRUE)))
})
