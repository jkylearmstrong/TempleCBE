# Tests for run_checkpointed_sweep() (item #33): the shared engine behind the
# checkpointed sweeps in missforest_sweep_mtry(), missranger_sweep_mtry() and
# nested_cv_coxnet(). These tests use a trivial compute function so they run
# in milliseconds and need no model fits; the sweep functions themselves are
# covered by their own test files.
#
# Checkpoint file naming, fingerprints, run fingerprint and metadata all come
# from the caller, so these tests pin the helper's cache semantics: a cache
# hit returns the stored value without recomputing, a fingerprint mismatch
# recomputes, and a task outside the shard returns NULL without computing
# (unless its checkpoint is already on disk, in which case the cached value
# is returned -- the cache check precedes the shard check, as in the
# original per-site sweep blocks).

test_that("run_checkpointed_sweep() maps compute over indices without a checkpoint dir", {
  calls <- integer()
  compute <- function(idx) {
    calls <<- c(calls, idx)
    idx * 10L
  }
  res <- run_checkpointed_sweep(compute, n = 3, names = c("a", "b", "c"))
  expect_equal(res, list(a = 10L, b = 20L, c = 30L))
  expect_equal(calls, 1:3)
})

test_that("run_checkpointed_sweep() writes one checkpoint per task and reuses them", {
  tmp <- file.path(tempdir(), "ckpt_sweep_reuse")
  unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  calls <- integer()
  compute <- function(idx) {
    calls <<- c(calls, idx)
    paste0("value-", idx)
  }
  fps <- c("fp-one", "fp-two", "fp-three")
  args <- list(
    compute = compute, n = 3, checkpoint_dir = tmp,
    ckpt_file = function(idx) file.path(tmp, sprintf("task_%03d.rds", idx)),
    fingerprint = fps, run_fingerprint = "run-1",
    meta = function(idx) list(fn = "test_sweep", index = idx)
  )
  res1 <- do.call(run_checkpointed_sweep, args)
  expect_equal(res1, list("value-1", "value-2", "value-3"))
  expect_equal(calls, 1:3)
  expect_equal(
    sort(list.files(tmp)),
    c("task_001.rds", "task_002.rds", "task_003.rds")
  )
  # Second run: every task is a cache hit, compute is never called.
  calls <- integer()
  res2 <- do.call(run_checkpointed_sweep, args)
  expect_equal(res2, res1)
  expect_equal(calls, integer())
})

test_that("run_checkpointed_sweep() recomputes on a fingerprint mismatch", {
  tmp <- file.path(tempdir(), "ckpt_sweep_mismatch")
  unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  calls <- integer()
  compute <- function(idx) {
    calls <<- c(calls, idx)
    idx
  }
  base_args <- list(
    compute = compute, n = 2, checkpoint_dir = tmp,
    ckpt_file = function(idx) file.path(tmp, sprintf("task_%03d.rds", idx)),
    run_fingerprint = "run-1",
    meta = list(fn = "test_sweep")
  )
  do.call(run_checkpointed_sweep, c(base_args, list(fingerprint = c("fp-a", "fp-b"))))
  expect_equal(calls, 1:2)
  # Only task 2 changes fingerprint: task 1 is a hit, task 2 recomputes.
  calls <- integer()
  res <- do.call(run_checkpointed_sweep, c(base_args, list(fingerprint = c("fp-a", "fp-CHANGED"))))
  expect_equal(res, list(1L, 2L))
  expect_equal(calls, 2L)
})

test_that("run_checkpointed_sweep() returns NULL without computing for inactive shard tasks", {
  tmp <- file.path(tempdir(), "ckpt_sweep_shard")
  unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  calls <- integer()
  compute <- function(idx) {
    calls <<- c(calls, idx)
    idx
  }
  args <- list(
    compute = compute, n = 3, checkpoint_dir = tmp,
    ckpt_file = function(idx) file.path(tmp, sprintf("task_%03d.rds", idx)),
    fingerprint = c("fp-a", "fp-b", "fp-c"), run_fingerprint = "run-1",
    meta = list(fn = "test_sweep"),
    active_mask = c(TRUE, FALSE, TRUE)
  )
  res <- do.call(run_checkpointed_sweep, args)
  expect_equal(res, list(1L, NULL, 3L))
  expect_equal(calls, c(1L, 3L))
  expect_false(file.exists(file.path(tmp, "task_002.rds")))
  # A cached value is still returned for an inactive task: the cache check
  # precedes the shard check.
  calls <- integer()
  res2 <- do.call(run_checkpointed_sweep, args)
  expect_equal(res2, list(1L, NULL, 3L))
  expect_equal(calls, integer())
})

test_that("run_checkpointed_sweep() skips writing a checkpoint for NULL results when asked", {
  tmp <- file.path(tempdir(), "ckpt_sweep_null")
  unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  compute <- function(idx) if (idx == 2L) NULL else idx
  base_args <- list(
    compute = compute, n = 2, checkpoint_dir = tmp,
    ckpt_file = function(idx) file.path(tmp, sprintf("task_%03d.rds", idx)),
    fingerprint = c("fp-a", "fp-b"), run_fingerprint = "run-1",
    meta = list(fn = "test_sweep")
  )
  res <- do.call(run_checkpointed_sweep, c(base_args, list(skip_write_if_null = TRUE)))
  expect_equal(res, list(1L, NULL))
  expect_true(file.exists(file.path(tmp, "task_001.rds")))
  expect_false(file.exists(file.path(tmp, "task_002.rds")))
})

test_that("run_checkpointed_sweep() parallel dispatch matches sequential results", {
  # Default (sequential) future plan: future_map runs in-process, so this
  # exercises the furrr dispatch without starting a cluster.
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  compute <- function(idx) idx * 2L
  seq_res <- run_checkpointed_sweep(compute, n = 3, names = c("a", "b", "c"))
  par_res <- run_checkpointed_sweep(
    compute, n = 3, parallel = TRUE, furrr_seed = TRUE, names = c("a", "b", "c"),
    parallel_reason = "to run the test sweep in parallel."
  )
  expect_equal(par_res, seq_res)
})
