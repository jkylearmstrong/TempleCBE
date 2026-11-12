# Tests for the fresh-review survival-core fixes (items 1-25, 27-29, 31-51,
# 136-137 of TempleCBE-gaps-and-features-2026-11-11.md, Part 1 section A).

test_that("tbl_tune_coef prop_selected is per term fit, not per split (item 1)", {
  # Two splits x two alphas = 4 fits per term; x1 is selected in 3 of them,
  # x2 in 1. Dividing by the number of splits (2) would give 1.5 and 0.5.
  df <- tibble::tibble(
    inner_resamples_splits = rep(c("s1", "s2"), each = 4),
    alpha = rep(c(0, 0, 1, 1), 2),
    IBS = c(0.20, 0.21, 0.19, 0.22, 0.20, 0.21, 0.19, 0.22),
    term = rep(c("x1", "x2"), 4),
    estimate = c(0.5, 0, 0.4, 0.3, 0, 0, 0.6, 0)
  )
  out <- tbl_tune_coef(df)
  expect_equal(out$prop_selected[out$term == "x1"], 0.75)
  expect_equal(out$prop_selected[out$term == "x2"], 0.25)
})

test_that("tbl_tune_coef and plot helpers drop NA terms (item 2)", {
  df <- tibble::tibble(
    inner_resamples_splits = "s1",
    alpha = 0,
    IBS = 0.2,
    term = c("x1", NA_character_),
    estimate = c(0.5, 0.1)
  )
  out <- tbl_tune_coef(df)
  expect_false(anyNA(out$term))
  expect_equal(nrow(out), 1L)
})

test_that("validate_start_stop rejects empty data, bad status and negative starts (items 3, 5)", {
  empty <- data.frame(id = integer(), tstart = numeric(), tstop = numeric(), status = integer())
  v <- validate_start_stop(empty, id = "id", start = "tstart", stop = "tstop", status = "status")
  expect_false(v$valid)
  expect_true(any(grepl("no rows", attr(v, "details")$issues)))

  bad <- data.frame(
    id = c(1L, 1L, 2L), tstart = c(0, 5, -1), tstop = c(5, 10, 4), status = c(0L, 2L, 1L)
  )
  v <- validate_start_stop(bad, id = "id", start = "tstart", stop = "tstop", status = "status")
  expect_false(v$valid)
  expect_true(any(grepl("status", attr(v, "details")$issues)))
  expect_true(any(grepl("negative", attr(v, "details")$issues)))
})

test_that("subject_keys errors on NA subject/start/stop/status (item 6)", {
  y <- survival::Surv(c(0, 0), c(5, 5), c(0L, 1L), type = "counting")
  expect_error(subject_keys(y, c(1L, NA_integer_)), "missing values")
})

test_that("recurrent_cox validates max_events and min_stratum_events (items 4, 8)", {
  d <- data.frame(
    id = rep(1:4, each = 2), tstart = rep(c(0, 5), 4), tstop = rep(c(5, 10), 4),
    status = c(0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L), x = rnorm(8)
  )
  expect_error(recurrent_cox(d, "x", max_events = 0), "max_events")
  expect_error(recurrent_cox(d, "x", min_stratum_events = 0), "min_stratum_events")
})

test_that("recurrent_intervals drops NA rows with a warning and keeps types on empty input (item 9)", {
  events <- data.frame(id = c(1L, 1L, NA_integer_), time = c(5, 10, NA))
  followup <- data.frame(id = 1L, end = 20)
  expect_warning(
    out <- recurrent_intervals(events, followup, id = "id", time = "time", end = "end"),
    "dropped"
  )
  # Two events for one subject make three intervals (two events + final
  # censored interval to the end of follow-up); the NA row is dropped.
  expect_equal(nrow(out), 3L)
  empty <- recurrent_intervals(events[0, ], followup[0, ], id = "id", time = "time", end = "end")
  expect_true(is.numeric(empty$tstart))
  expect_true(is.integer(empty$status))
})

test_that("truncate_start_stop does not fabricate events for reversed factor levels (item 34)", {
  d <- data.frame(
    id = rep(1L, 2), tstart = c(0, 5), tstop = c(5, 10),
    status = factor(c("0", "1"), levels = c("1", "0")), x = c(1, 2)
  )
  out <- truncate_start_stop(d, at = 7, status = "status")
  # The interval cut at 7 must be censored (the "0" level), not an event:
  # with levels c("1", "0") the old code took the first level ("1") and
  # fabricated an event.
  expect_equal(as.character(out$status[out$tstart == 5]), "0")
})

test_that("km_cutpoint_search stops with zero events (item 10)", {
  d <- data.frame(time = 1:10, status = 0L, x = 1:10)
  expect_error(km_cutpoint_search(d, "time", "status", "x"), "event")
})

test_that("calibration_table reports NA intercept/slope for a single-class outcome (item 11)", {
  # A one-level factor is rejected by validation_inputs (factor() drops the
  # unused level), so the single-class outcome is passed as 0/1.
  truth <- rep(0, 20)
  pred <- runif(20)
  tab <- calibration_table(truth, pred)
  m <- attr(tab, "metrics")
  expect_true(is.na(m$intercept))
  expect_true(is.na(m$slope))
  expect_false(is.na(m$brier))
})

test_that("decision_curve warns when horizon exceeds follow-up (item 12)", {
  y <- survival::Surv(survival::lung$time, survival::lung$status - 1)
  pred <- runif(length(y))
  expect_warning(decision_curve(y, pred, horizon = max(y[, "time"]) + 100), "horizon")
})

test_that("cbe_cox_table maps '<x' p-values below x (item 20)", {
  # p_as_number() is local to cbe_cox_table(): exercise it through the table.
  # "<0.001" parsed as 0.001 would earn two stars; as 0.000999 it earns three.
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))
  fit <- cox_single(lung, outcome = "outcome", feature = "sex_f")
  fit$table$p.value[fit$table$Role != "Reference"] <- "<0.001"
  tab <- cbe_cox_table(fit, significance = TRUE)
  expect_equal(tab$sig[tab$Role != "Reference"], "***")
})

test_that("km_single labels the numeric HR as continuous and uses the unrounded HR (item 21)", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- km_single(lung, outcome = "outcome", feature = "age")
  expect_true("HR_continuous" %in% names(res$summary))
  expect_true(res$direction %in% c("increases", "decreases"))
})

test_that("cox_check does not flag NA proportional-hazards p-values as violated (item 22)", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
  res <- cox_check(fit)
  # NA p-values used to stay NA in zph_violated; they are now FALSE, never NA.
  expect_true(is.logical(res$zph_violated))
  expect_false(anyNA(res$zph_violated))
})

test_that("batch_cox_single keeps going when one feature fails (item 23)", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$status <- lung$status - 1
  lung$const <- 1
  out <- batch_cox_single(lung, "time", "status", c("age", "const"), parallel = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$status[out$feature == "age"], "ok")
  expect_false(out$status[out$feature == "const"] == "ok")
})

test_that("cox_single validates conf_level (item 35)", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  expect_error(cox_single(lung, outcome = "outcome", feature = "age", conf_level = 1.5), "conf_level")
  expect_error(cox_single(lung, outcome = "outcome", feature = "age", conf_level = c(0.9, 0.95)), "conf_level")
  # cox_multi() shares the ci_col pattern and validates the same way.
  expect_error(cox_multi(lung, features = "age", conf_level = 2), "conf_level")
  expect_error(cox_multi(lung, features = "age", conf_level = NA_real_), "conf_level")
})

test_that("select_candidate_models validates ntile and stops on empty selection (item 45)", {
  tr <- tibble::tibble(alpha = c(0, 0.5, 1), IBS = c(0.2, 0.25, 0.3))
  expect_error(select_candidate_models(tr, "x1", ntile = 2), "ntile")
  expect_error(select_candidate_models(tr, "x1", ntile = 0), "ntile")
  tr_na <- tibble::tibble(alpha = c(0, 0.5), IBS = c(NA_real_, NA_real_))
  expect_error(select_candidate_models(tr_na, "x1", by = "alpha"), "No candidate")
})

test_that("compare_runs treats NA diffs as unassessable, not reproducible (item 43)", {
  r1 <- tibble::tibble(model = c("a", "b"), IBS = c(0.20, NA_real_), lambda = c(0.1, 0.2))
  r2 <- tibble::tibble(model = c("a", "b"), IBS = c(0.20, 0.30), lambda = c(0.1, 0.2))
  out <- compare_runs(r1, r2)
  expect_false(out$reproducible[out$model == "b"])
  expect_true(out$reproducible[out$model == "a"])
  expect_true(attr(out, "comparison_summary")$n_unassessable >= 1L)

  r1na <- tibble::tibble(model = "a", IBS = NA_real_, lambda = 0.1)
  r2na <- tibble::tibble(model = "a", IBS = NA_real_, lambda = 0.1)
  out2 <- compare_runs(r1na, r2na)
  expect_false(out2$reproducible)
  expect_true(is.na(attr(out2, "comparison_summary")$max_ibs_diff))
})

test_that("compare_tuning_runs nearest never matches across formulas (item 44)", {
  t1 <- tibble::tibble(alpha = c(0, 0.5), formula = c("x1", "x2"), IBS = c(0.2, 0.25))
  t2 <- tibble::tibble(alpha = 0.1, formula = "x1", IBS = 0.21)
  expect_warning(
    out <- compare_tuning_runs(t1, t2, match = "nearest"),
    "no match"
  )
  expect_true(is.na(out$mean_ibs_run2[out$formula == "x2"]))
  expect_false(is.na(out$mean_ibs_run2[out$formula == "x1"]))
})

test_that("balance_smd drops unused factor levels (item 33)", {
  d <- data.frame(
    group = factor(rep(c("A", "B"), each = 10)),
    x = factor(rep(c("a", "b"), 10), levels = c("a", "b", "never")),
    y = rnorm(20)
  )
  out <- balance_smd(d, "group", vars = c("x", "y"))
  expect_false(any(grepl("never", out$variable)))
  expect_true(all(!is.na(out$smd[out$variable %in% c("x = a", "x = b")])))
})

test_that("estimate_runtime validates workers and n_machines (item 48)", {
  expect_error(estimate_runtime(function() 1, list(), workers = NA), "workers")
  expect_error(estimate_runtime(function() 1, list(), workers = 0), "workers")
  expect_error(estimate_runtime(function() 1, list(), n_machines = NA), "n_machines")
})

test_that("collect_checkpoints judges split files against n_splits, not n_tasks (item 39)", {
  dir <- tempfile("ckpt_split_")
  dir.create(dir)
  for (i in 1:2) {
    v <- tibble::tibble(alpha = 0.5, IBS = 0.2 + i * 0.01)
    write_verified_checkpoint(
      v, file.path(dir, sprintf("tune_split_s%d.rds", i)),
      fingerprint = paste0("fp", i), run_fingerprint = "run1",
      meta = list(fn = "tune_alpha_by_split", split_idx = i, split_id = paste0("s", i),
                  n_splits = 2L, n_tasks = 10L)
    )
  }
  out <- collect_checkpoints(dir, type = "tune")
  expect_equal(nrow(out), 2L)
})

test_that("clean_checkpoints keeps fingerprinted list and empty data-frame checkpoints (item 40)", {
  dir <- tempfile("ckpt_clean_")
  dir.create(dir)
  write_verified_checkpoint(
    list(a = 1), file.path(dir, "list_val.rds"),
    fingerprint = "fp1", run_fingerprint = "run1", meta = list(fn = "workflow_map_checkpointed")
  )
  write_verified_checkpoint(
    tibble::tibble(), file.path(dir, "empty_df.rds"),
    fingerprint = "fp2", run_fingerprint = "run1", meta = list(fn = "score_outer")
  )
  out <- clean_checkpoints(dir, keep = "current", run_fingerprint = "run1", dry_run = TRUE)
  expect_equal(nrow(out[out$action == "kept", ]), 2L)
})

test_that("score_outer stops clearly on an empty shard (item 46)", {
  skip_if_no_coxnet_deps()
  sim <- sim_counting(n = 40, seed = 2)
  rec <- recipes::recipe(~ ., data = sim) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable")
  split <- rsample::group_initial_split(sim, group = subject_id)
  grid <- data.frame(model = "m1", alpha = 0.5)
  expect_error(
    score_outer(split, grid, recipe = rec, shard = c(3L, 3L), feature_names = c("x1", "x2")),
    "no tasks assigned"
  )
})

test_that("tune_alpha_by_split stops clearly on an empty shard (item 19)", {
  skip_if_no_coxnet_deps()
  sim <- sim_counting(n = 40, seed = 2)
  rec <- recipes::recipe(~ ., data = sim) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable")
  rset <- rsample::group_vfold_cv(sim, group = subject_id, v = 2)
  expect_error(
    tune_alpha_by_split(rset, recipe = rec, feature_names = c("x1", "x2"),
                        alphas = 0.5, shard = c(5L, 5L), calibrate = FALSE),
    "no tasks assigned"
  )
})

test_that("cv_coxnet exposes parallel_over and nested_cv_coxnet exposes loco_mp_B (items 36, 38)", {
  expect_true("parallel_over" %in% names(formals(cv_coxnet.data.frame)))
  expect_true("parallel_over" %in% names(formals(cv_coxnet.formula)))
  expect_true("parallel_over" %in% names(formals(cv_coxnet.matrix)))
  expect_true("parallel_over" %in% names(formals(cv_coxnet.recipe)))
  expect_true("loco_mp_B" %in% names(formals(nested_cv_coxnet)))
  expect_equal(formals(nested_cv_coxnet)$loco_mp_B, 30L)
  expect_true("recurrent" %in% names(formals(glmnet_IBS)))
})

test_that("metric_set_names recovers metric names from a metric set (item 13)", {
  skip_if_not_installed("yardstick")
  ms <- yardstick::metric_set(yardstick::brier_survival_integrated)
  expect_equal(metric_set_names(ms), "brier_survival_integrated")
  expect_null(metric_set_names(NULL))
})

test_that("recurrent_intervals always keeps the first event, even within min_gap (item 37)", {
  # Maintainer decision (2026-11-11): the first event has no previous event,
  # so min_gap cannot drop it.
  out <- suppressWarnings(recurrent_intervals(
    data.frame(id = c(1, 1, 1, 2, 2), time = c(2, 3, 30, 1, 40)),
    data.frame(id = 1:2, end = c(50, 60)),
    min_gap = 5
  ))
  # Subject 1: first event at 2 kept (2 <= min_gap); second at 3 dropped
  # (within 5 of 2); third at 30 kept. Subject 2: first event at 1 kept.
  expect_equal(out$id, c(1, 1, 1, 2, 2, 2))
  expect_equal(out$tstop, c(2, 30, 50, 1, 40, 60))
  expect_equal(out$status, c(1, 1, 0, 1, 1, 0))
  # A lone first event within min_gap is kept, with no warning at all.
  expect_no_warning(
    out1 <- recurrent_intervals(
      data.frame(id = 1, time = 2),
      data.frame(id = 1, end = 50),
      min_gap = 5
    )
  )
  expect_equal(out1$tstop, c(2, 50))
  expect_equal(out1$status, c(1, 0))
})

test_that("truncate_start_stop picks the censored level by meaning and refuses to guess (item 34)", {
  mk <- function(lv) data.frame(
    id = 1L, tstart = c(0, 5), tstop = c(5, 10),
    status = factor(c(lv[2], lv[1]), levels = lv)
  )
  out <- truncate_start_stop(mk(c("censored", "event")), at = 7)
  expect_equal(as.character(out$status[out$tstart == 5]), "censored")
  # an unrecognisable factor stops when an interval has to be cut ...
  expect_error(truncate_start_stop(mk(c("yes", "no")), at = 7), "censored")
  # ... but not when nothing is cut
  expect_equal(nrow(truncate_start_stop(mk(c("yes", "no")), at = 20)), 2L)
})
