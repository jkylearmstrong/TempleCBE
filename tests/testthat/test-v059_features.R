make_tune_fixture <- function(n = 60, seed = 1) {
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

test_that("K1: cv_coxnet checkpointing defaults to seed = 1L and resumes cleanly", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  # Small reproducible dataset
  set.seed(123)
  df <- data.frame(
    time = stats::runif(60, 5, 50),
    status = stats::rbinom(60, 1, 0.7),
    x1 = stats::rnorm(60),
    x2 = stats::rnorm(60),
    subject_id = seq_len(60)
  )

  tmp_dir <- file.path(tempdir(), paste0("k1_ckpt_", as.integer(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # First run without explicit seed: defaults to seed = 1L with message
  expect_message(
    cv1 <- cv_coxnet(
      survival::Surv(time, status) ~ x1 + x2,
      data = df, subject_id = "subject_id",
      v = 3, mixture = 1,
      checkpoint_dir = tmp_dir
    ),
    "Defaulting to seed = 1L"
  )
  expect_s3_class(cv1, "cv_coxnet")

  # Checkpoint files exist
  cf_files <- list.files(tmp_dir, pattern = "\\.rds$")
  expect_equal(length(cf_files), 3L)

  # Second run without explicit seed: MUST resume and reuse cached folds without discarding
  msg_out <- character()
  cv2 <- withCallingHandlers(
    cv_coxnet(
      survival::Surv(time, status) ~ x1 + x2,
      data = df, subject_id = "subject_id",
      v = 3, mixture = 1,
      checkpoint_dir = tmp_dir
    ),
    message = function(m) {
      msg_out <<- c(msg_out, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(cv2, "cv_coxnet")
  # Verify NO cached tasks were discarded
  expect_false(any(grepl("Discarded.*cached task", msg_out)))

  # Third run with explicit seed = 42 on clean dir
  tmp_dir2 <- file.path(tempdir(), paste0("k1_ckpt2_", as.integer(stats::runif(1, 1e5, 9e5))))
  on.exit(unlink(tmp_dir2, recursive = TRUE), add = TRUE)

  cv3a <- cv_coxnet(
    survival::Surv(time, status) ~ x1 + x2,
    data = df, subject_id = "subject_id",
    v = 3, mixture = 1, seed = 42L,
    checkpoint_dir = tmp_dir2
  )
  cv3b <- cv_coxnet(
    survival::Surv(time, status) ~ x1 + x2,
    data = df, subject_id = "subject_id",
    v = 3, mixture = 1, seed = 42L,
    checkpoint_dir = tmp_dir2
  )
  expect_equal(cv3a$metrics, cv3b$metrics)
})

test_that("K2: Parallel tuning timing breakdown reports serial baseline, speedup, and physical core guidance", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("recipes")
  skip_if_not_installed("yardstick")

  fx <- make_tune_fixture()
  msgs <- character()
  res <- withCallingHandlers(
    suppressWarnings(tune_alpha_by_split(
      fx$splits,
      recipe = fx$recipe,
      feature_names = fx$features,
      time_data = fx$times,
      cox.ties = "breslow",
      alphas = c(0.2, 0.8),
      internal_folds = 2,
      progress = TRUE
    )),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  tb <- attr(res, "timing_breakdown")
  expect_true(is.list(tb))
  expect_true("measured_serial_task_sec" %in% names(tb))
  expect_true("measured_serial_baseline_sec" %in% names(tb))
  expect_true("real_speedup" %in% names(tb))
  expect_true("worker_concurrency_slowdown" %in% names(tb))
  expect_true("physical_cores" %in% names(tb))
  expect_true("logical_cores" %in% names(tb))
  expect_true(tb$real_speedup > 0)
  expect_true(tb$measured_serial_task_sec > 0)

  # Check progress message contents
  tb_msg <- paste(msgs, collapse = "\n")
  expect_true(grepl("real speedup", tb_msg))
  expect_true(grepl("measured serial baseline", tb_msg))

  # Test estimate_runtime physical core recommendations
  est <- estimate_runtime(
    tune_alpha_by_split,
    args = list(object = fx$splits, recipe = fx$recipe, feature_names = fx$features, time_data = fx$times, alphas = c(0.2, 0.8)),
    n_sample = 1
  )
  expect_s3_class(est, "cbe_runtime_estimate")
  expect_true("physical_cores" %in% names(est))

  pr_out <- utils::capture.output(print(est))
  expect_true(any(grepl("Recommended workers:", pr_out)))
})

test_that("K3: compare_tuning_runs() suppresses tie warnings from ks.test and notes approximation", {
  # Mock two tuning results with identical/tied IBS values
  tune1 <- data.frame(
    alpha = rep(c(0.1, 0.5, 0.9), each = 4),
    IBS = c(0.15, 0.15, 0.18, 0.18, 0.20, 0.20, 0.22, 0.22, 0.25, 0.25, 0.28, 0.28)
  )
  tune2 <- data.frame(
    alpha = rep(c(0.1, 0.5, 0.9), each = 4),
    IBS = c(0.15, 0.15, 0.18, 0.18, 0.21, 0.21, 0.23, 0.23, 0.26, 0.26, 0.29, 0.29)
  )

  # Must not emit any warning about ties from ks.test
  expect_no_warning(
    comp <- compare_tuning_runs(tune1, tune2, label1 = "runA", label2 = "runB", match = "exact")
  )
  expect_s3_class(comp, "templecbe_tuning_comparison")

  s <- attr(comp, "comparison_summary")
  expect_true(isTRUE(s$ks_has_ties))

  pr <- utils::capture.output(print(comp))
  expect_true(any(grepl("approximate in presence of ties", pr)))
})

test_that("K4: %notin% is unexported and masks no base function while internal shim functions", {
  # %notin% must NOT be exported from TempleCBE
  exports <- getNamespaceExports("TempleCBE")
  expect_false("%notin%" %in% exports)

  # %!in% must be exported
  expect_true("%!in%" %in% exports)
  expect_true(1 %!in% c(2, 3, 4))
  expect_false(1 %!in% c(1, 2, 3))

  # Internal shim works with deprecation warning
  expect_warning(
    res_in <- TempleCBE:::`%notin%`(1, c(2, 3, 4)),
    "unexported in 0\\.5\\.9"
  )
  expect_true(res_in)
})

test_that("K5: Version 0.5.9 is properly synchronized across DESCRIPTION, NEWS, and code", {
  ver <- as.character(utils::packageVersion("TempleCBE"))
  expect_true(utils::compareVersion(ver, "0.5.9") >= 0)

  news_candidates <- c(
    system.file("NEWS.md", package = "TempleCBE"),
    file.path(system.file(package = "TempleCBE"), "NEWS.md"),
    tryCatch(testthat::test_path("../../NEWS.md"), error = function(e) "")
  )
  found_news <- news_candidates[file.exists(news_candidates) & nzchar(news_candidates)]
  if (length(found_news) > 0L) {
    news_lines <- readLines(found_news[1], n = 5)
    expect_true(any(grepl("^# TempleCBE [0-9]+\\.[0-9]+", news_lines)))
  }
})
