# ==============================================================================
# Unit Tests for TempleCBE v0.5.7 Gaps/Bugs (D1-D3) & Features (E1-E7)
# ==============================================================================

make_v057_surv_data <- function(n_subj = 36, seed = 301, all_events = FALSE) {
  withr::local_seed(seed)
  do.call(rbind, lapply(seq_len(n_subj), function(i) {
    k <- sample(1:2, 1)
    stops <- c(6, 15)[seq_len(k)]
    x1 <- stats::rnorm(1)
    ev <- if (isTRUE(all_events)) 1L else stats::rbinom(1, 1, stats::plogis(1.2 * x1))
    data.frame(
      id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), ev),
      x1 = x1 + stats::rnorm(k, sd = 0.1),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

# ------------------------------------------------------------------------------
# D1: Flat parallelism worker bundle caching & chunk_size
# ------------------------------------------------------------------------------

test_that("D1: shared worker bundle caches splits/recipe once per worker and chunk_size works", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "furrr")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v057_surv_data(n_subj = 32, seed = 401)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable") |>
    recipes::step_normalize(all_numeric_predictors())
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  # Verify worker bundle compaction and hydration
  bundle_info <- create_shared_worker_bundle(
    splits = splits$splits,
    dots = list(recipe = rec, id_col = "id", start_col = "tstart", stop_col = "tstop", status_col = "status"),
    run_fp = "test_d1_run_fp"
  )
  on.exit({
    if (!is.null(bundle_info$bundle_file) && file.exists(bundle_info$bundle_file)) {
      unlink(bundle_info$bundle_file)
    }
    if (exists("test_d1_run_fp", envir = .worker_cache, inherits = FALSE)) {
      rm(list = "test_d1_run_fp", envir = .worker_cache)
    }
  }, add = TRUE)

  # Compact splits strip duplicate `data` pointers
  expect_true(all(vapply(bundle_info$payload$compact_splits, function(cs) isTRUE(cs$is_compact) && is.null(cs$split$data), logical(1))))

  hydrated <- get_worker_bundle("test_d1_run_fp", bundle_file = bundle_info$bundle_file)
  expect_equal(nrow(hydrated$splits[[1]]$data), nrow(long))

  prep1 <- get_worker_prepared_split(hydrated, 1L)
  prep2 <- get_worker_prepared_split(hydrated, 1L)
  expect_true(isTRUE(prep1$rng_free))
  expect_identical(prep1, prep2)

  # chunk_size parameter in summarize_tune_results and score_outer produces identical numerics
  res_default <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.2, 0.5, 0.8),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    seed = 67025
  )
  res_chunked <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.2, 0.5, 0.8),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    chunk_size = 2L,
    seed = 67025
  )
  expect_equal(res_default$IBS, res_chunked$IBS)
  expect_equal(res_default$estimate, res_chunked$estimate)
})

# ------------------------------------------------------------------------------
# D2: seed_scheme in run manifest and checkpoint fingerprint + NEWS notice
# ------------------------------------------------------------------------------

test_that("D2: seed_scheme is recorded in write_run_manifest() and verified by checkpoints", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "jsonlite")) {
    skip_if_not_installed(pkg)
  }

  expect_equal(current_seed_scheme(), "v0.5.6")

  # 1. Run manifest records seed_scheme
  tmp_manifest_dir <- file.path(tempdir(), paste0("manifest_d2_", floor(stats::runif(1, 1e5, 9e5))))
  dir.create(tmp_manifest_dir, recursive = TRUE)
  on.exit(unlink(tmp_manifest_dir, recursive = TRUE), add = TRUE)

  m_path <- write_run_manifest(tmp_manifest_dir, data.frame(file = "a.qmd", status = "success"), seed = 67025)
  m_json <- jsonlite::read_json(m_path)
  expect_equal(m_json$seed_scheme, "v0.5.6")
  expect_equal(m_json$seed, 67025)

  # 2. Checkpoints record seed_scheme and reject old seed_scheme (e.g. v0.5.5)
  long <- make_v057_surv_data(n_subj = 30, seed = 402)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  ckpt_dir <- file.path(tempdir(), paste0("ckpt_d2_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_dir, recursive = TRUE), add = TRUE)

  res <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.3, 0.7),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    checkpoint_dir = ckpt_dir,
    seed = 67025
  )
  expect_equal(attr(res, "seed_scheme"), "v0.5.6")

  rds_files <- list.files(ckpt_dir, pattern = "^tune_task_split_.*\\.rds$", full.names = TRUE)
  expect_true(length(rds_files) > 0L)
  obj1 <- readRDS(rds_files[1])
  expect_equal(attr(obj1, "seed_scheme"), "v0.5.6")

  # Stamp an old v0.5.5 seed_scheme onto one checkpoint file
  attr(obj1, "seed_scheme") <- "v0.5.5"
  meta1 <- attr(obj1, "checkpoint_meta")
  meta1$seed_scheme <- "v0.5.5"
  attr(obj1, "checkpoint_meta") <- meta1
  saveRDS(obj1, rds_files[1])

  # collect_checkpoints must refuse to mix v0.5.5 and v0.5.6 checkpoints
  expect_error(
    collect_checkpoints(ckpt_dir, type = "tune"),
    "seed_scheme"
  )

  # summarize_tune_results with checkpoint_mismatch = "error" must also stop on seed_scheme mismatch
  expect_error(
    tune_alpha_by_split(
      object = splits,
      alphas = c(0.3, 0.7),
      recipe = rec,
      feature_names = c("x1", "x2", "x3"),
      time_data = times,
      internal_folds = 3,
      checkpoint_dir = ckpt_dir,
      checkpoint_mismatch = "error",
      seed = 67025
    ),
    "seed_scheme|mismatch"
  )
})

# ------------------------------------------------------------------------------
# D3: glmnet_IBS() with missing feature_names stops immediately
# ------------------------------------------------------------------------------

test_that("D3: glmnet_IBS() stops immediately when no requested feature_names exist in baked data", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v057_surv_data(n_subj = 30, seed = 403)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  expect_error(
    glmnet_IBS(
      object = splits$splits[[1]],
      alpha = 0.5,
      recipe = rec,
      feature_names = c("missing_var_a", "missing_var_b"),
      time_data = times,
      internal_folds = 3
    ),
    "No requested `feature_names` were found in the baked training data"
  )
})

# ------------------------------------------------------------------------------
# E3: compare_censoring_weights() skips duplicate fit when no censored subjects
# ------------------------------------------------------------------------------

test_that("E3: compare_censoring_weights() runs score_outer() only once when training data has no censoring", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  # Multi-interval start/stop data where every subject eventually has an event (n_censored == 0)
  uncens_df <- make_v057_surv_data(n_subj = 32, seed = 404, all_events = TRUE)
  rec <- recipes::recipe(~ ., data = uncens_df) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(uncens_df, group = id, v = 2)
  times <- unique(uncens_df[, c("tstart", "tstop")])

  expect_false(has_censored_subjects_in_splits(splits, recipe = rec))

  grid <- tibble::tibble(
    model = c("m1", "m2"),
    alpha = c(0.2, 0.8),
    formula = "x1 + x2 + x3"
  )

  withr::local_options(templecbe_suppress_frequency_limit = TRUE)
  msgs <- testthat::capture_messages({
    comp <- compare_censoring_weights(
      object = splits,
      grid = grid,
      recipe = rec,
      feature_names = c("x1", "x2", "x3"),
      time_data = times,
      internal_folds = 3,
      seed = 42
    )
  })

  expect_true(isTRUE(attr(comp, "single_fit_no_censoring")))
  expect_equal(attr(comp, "n_score_outer_runs"), 1L)
  expect_equal(comp$ibs_none, comp$ibs_ipcw)
  expect_equal(comp$rank_diff, rep(0, nrow(comp)))
  expect_true(any(grepl("running score_outer\\(\\) once", msgs)))
})

# ------------------------------------------------------------------------------
# E5: clean_checkpoints() and checkpoint_mismatch = "overwrite"
# ------------------------------------------------------------------------------

test_that("E5: clean_checkpoints() removes stale checkpoints and checkpoint_mismatch = 'overwrite' overwrites in place", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v057_surv_data(n_subj = 30, seed = 405)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  ckpt_dir <- file.path(tempdir(), paste0("ckpt_e5_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(ckpt_dir, recursive = TRUE), add = TRUE)

  # Initial run with 3 alphas
  res1 <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.2, 0.5, 0.8),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    checkpoint_dir = ckpt_dir,
    seed = 50
  )

  # Write an artificial stale checkpoint from an older run fingerprint
  stale_file <- file.path(ckpt_dir, "tune_task_split_99_item_99.rds")
  stale_df <- tibble::tibble(IBS = 0.25, lambda = 0.1, term = "x1", estimate = 0.1, alpha = 0.5)
  attr(stale_df, "checkpoint_fingerprint") <- "old_task_fp"
  attr(stale_df, "checkpoint_run_fingerprint") <- "old_run_fp"
  attr(stale_df, "seed_scheme") <- "v0.5.5"
  saveRDS(stale_df, stale_file)
  Sys.setFileTime(stale_file, Sys.time() - 3600)

  # Dry-run cleanup identifies 1 stale file to remove and 8 current files (6 task + 2 split) to keep
  dry <- clean_checkpoints(ckpt_dir, keep = "current", dry_run = TRUE)
  expect_s3_class(dry, "cbe_clean_checkpoints")
  expect_equal(attr(dry, "n_kept"), 8L)
  expect_equal(attr(dry, "n_removed"), 1L)
  expect_true(file.exists(stale_file))

  # Actual cleanup removes the stale file
  cleaned <- clean_checkpoints(ckpt_dir, keep = "current", dry_run = FALSE)
  expect_equal(attr(cleaned, "n_removed"), 1L)
  expect_false(file.exists(stale_file))

  # Overwrite mode: changing feature_names with checkpoint_mismatch = "overwrite"
  # overwrites existing checkpoint files in place WITHOUT emitting a mismatch warning
  ow_warns <- testthat::capture_warnings(
    res_ow <- tune_alpha_by_split(
      object = splits,
      alphas = c(0.2, 0.5, 0.8),
      recipe = rec,
      feature_names = c("x1", "x2"),
      time_data = times,
      internal_folds = 3,
      checkpoint_dir = ckpt_dir,
      checkpoint_mismatch = "overwrite",
      seed = 50
    )
  )
  expect_false(any(grepl("mismatched", ow_warns, ignore.case = TRUE)))
  expect_false("x3" %in% res_ow$term)
})

# ------------------------------------------------------------------------------
# E6: Finish-time hook on_complete for long-running functions
# ------------------------------------------------------------------------------

test_that("E6: on_complete callback and templecbe.on_complete option fire on success and failure", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }

  long <- make_v057_surv_data(n_subj = 30, seed = 406)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role("id", "tstart", "tstop", "status", new_role = "id variable")
  splits <- rsample::group_vfold_cv(long, group = id, v = 2)
  times <- unique(long[, c("tstart", "tstop")])

  events <- list()
  hook_fn <- function(payload) {
    events[[length(events) + 1L]] <<- payload
  }

  res <- tune_alpha_by_split(
    object = splits,
    alphas = c(0.3, 0.7),
    recipe = rec,
    feature_names = c("x1", "x2", "x3"),
    time_data = times,
    internal_folds = 3,
    on_complete = hook_fn,
    seed = 77
  )
  expect_equal(length(events), 1L)
  expect_equal(events[[1]]$fn, "tune_alpha_by_split")
  expect_equal(events[[1]]$status, "completed")
  expect_true(is.numeric(events[[1]]$elapsed_sec))
  expect_s3_class(events[[1]]$result, "tbl_df")

  # Option-based hook on failure
  fail_events <- list()
  withr::with_options(
    list(templecbe.on_complete = function(p) {
      fail_events[[length(fail_events) + 1L]] <<- p
    }),
    {
      expect_error(
        tune_alpha_by_split(
          object = splits,
          alphas = c(0.5),
          recipe = rec,
          feature_names = c("nonexistent_feature"),
          time_data = times,
          internal_folds = 3,
          seed = 77
        )
      )
    }
  )
  expect_equal(length(fail_events), 1L)
  expect_equal(fail_events[[1]]$status, "failed")
  expect_s3_class(fail_events[[1]]$error, "error")
})

# ------------------------------------------------------------------------------
# E4: cbe_domain_children() per-domain expansion and failure isolation
# ------------------------------------------------------------------------------

test_that("E4: domain_children() expands domains, isolates failing domains, and records status", {
  skip_if_not_installed("knitr")

  child_tpl <- paste(
    "### Domain: {{domain}}",
    "```{r}",
    "if (domain == 'BadDomain') stop('Synthetic domain failure for testing')",
    "cat(sprintf('Successfully evaluated %s (val = %d)', domain, bonus_val))",
    "```",
    sep = "\n"
  )

  expect_warning(
    res <- domain_children(
      domains = c("Cardio", "BadDomain", "Renal"),
      child = child_tpl,
      bonus_val = list(Cardio = 10L, BadDomain = 20L, Renal = 30L),
      on_error = "continue"
    ),
    "BadDomain"
  )

  expect_s3_class(res, "cbe_domain_children")
  expect_equal(res$failed_domains, "BadDomain")
  expect_equal(res$succeeded_domains, c("Cardio", "Renal"))

  st <- generics::tidy(res)
  expect_s3_class(st, "tbl_df")
  expect_equal(nrow(st), 3L)
  expect_equal(st$status, c("succeeded", "failed", "succeeded"))
  expect_match(as.character(res), "Successfully evaluated Cardio \\(val = 10\\)")
  expect_match(as.character(res), "Successfully evaluated Renal \\(val = 30\\)")
  expect_match(as.character(res), "Synthetic domain failure for testing")
})

# ------------------------------------------------------------------------------
# E1, E2, & E7: cbe_nested_survival_run(), repro_appendix(), and rossi_data sharding
# ------------------------------------------------------------------------------

test_that("E1, E2 & E7: nested_survival_run() and repro_appendix() work end-to-end on rossi_data()", {
  for (pkg in c("glmnet", "survival", "rsample", "yardstick", "recipes", "ggplot2", "jsonlite")) {
    skip_if_not_installed(pkg)
  }

  raw_rossi <- rossi_data()
  rossi_sub <- raw_rossi[1:60, ]
  rossi_sub$id <- seq_len(nrow(rossi_sub))
  rossi_sub$tstart <- 0
  rossi_sub$tstop <- as.numeric(rossi_sub$week)
  rossi_sub$status <- as.integer(rossi_sub$arrest)
  rossi_sub <- rossi_sub[, c("id", "tstart", "tstop", "status", "fin", "age", "race", "wexp", "mar", "paro", "prio")]

  run_dir <- file.path(tempdir(), paste0("rossi_e1_", floor(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)

  # E1: Full nested survival run in one call
  run1 <- nested_survival_run(
    data = rossi_sub,
    id = "id",
    start = "tstart",
    stop = "tstop",
    status = "status",
    domain = "RossiRecidivism",
    run_dir = run_dir,
    feature_names = c("fin", "age", "race", "wexp", "mar", "paro", "prio"),
    formulas = c("fin + age + prio", "fin + age + race + wexp + mar + paro + prio"),
    outer_v = 2,
    inner_v = 2,
    alphas = c(0.25, 0.75),
    top_n = 2,
    internal_folds = 3,
    cox.ties = "breslow",
    seed = 67025
  )

  expect_s3_class(run1, "cbe_nested_survival_run")
  expect_false(run1$resumed)
  expect_equal(run1$seed_scheme, "v0.5.6")
  expect_true(file.exists(file.path(run_dir, "nested_survival_run.rds")))
  expect_true(file.exists(file.path(run_dir, "render_manifest.json")))

  # S3 methods: summary(), plot(), autoplot(), tidy()
  s_run <- summary(run1)
  expect_s3_class(s_run, "summary.cbe_nested_survival_run")
  expect_equal(s_run$overview$domain, "RossiRecidivism")

  p_imp <- plot(run1, type = "importance")
  expect_s3_class(p_imp, "ggplot")
  p_ibs <- ggplot2::autoplot(run1, type = "outer_ibs")
  expect_s3_class(p_ibs, "ggplot")

  td_imp <- generics::tidy(run1, component = "importance")
  expect_s3_class(td_imp, "tbl_df")
  expect_true(nrow(td_imp) > 0L)

  # Rerunning on the same run_dir resumes from the fingerprinted artifact
  run2 <- nested_survival_run(
    data = rossi_sub,
    id = "id",
    start = "tstart",
    stop = "tstop",
    status = "status",
    domain = "RossiRecidivism",
    run_dir = run_dir,
    feature_names = c("fin", "age", "race", "wexp", "mar", "paro", "prio"),
    formulas = c("fin + age + prio", "fin + age + race + wexp + mar + paro + prio"),
    outer_v = 2,
    inner_v = 2,
    alphas = c(0.25, 0.75),
    top_n = 2,
    internal_folds = 3,
    cox.ties = "breslow",
    seed = 67025
  )
  expect_true(run2$resumed)
  expect_equal(run1$outer_scores$IBS, run2$outer_scores$IBS)

  # E2: Self-rendering reproducibility appendix
  app <- repro_appendix(run = run1, data = list(rossi_sub = rossi_sub))
  expect_s3_class(app, "cbe_repro_appendix")
  expect_equal(app$seed_scheme, "v0.5.6")
  expect_true(nrow(app$package_versions) >= 5L)
  expect_true(nrow(app$data_hashes) >= 1L)
  expect_true(nrow(app$validation) == 1L)
  expect_true(nrow(app$checkpoint_fingerprints) >= 1L)
  expect_match(as.character(app), "Reproducibility Appendix")
  expect_match(as.character(app), "v0.5.6")
  expect_true(file.exists(repro_appendix_template()))
})
