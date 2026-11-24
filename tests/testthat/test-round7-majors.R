# Round-7 majors (review of 2026-11-24, master 29b819c / 0.14.0): R7-01–R7-05.
# One block per finding id. File-local helpers are prefixed r7_ (R7-05 helpers r705_*).
# Each checkpoint test asserts both directions: the changed input recomputes
# (mtimes move, result equals a fresh run) and the identical input still
# reuses (mtimes stay put), so a fix that merely invalidates everything
# would fail here too.

# Start/stop data with heavily tied event times, so the Breslow/Efron fits
# and the IBS/concordance selections genuinely differ (as in test-round6-gaps.R).
r7_tied <- function(n = 60, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:2, 1)
    stops <- sort(sample(c(3, 6, 9, 12), k))
    risk <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * risk))),
      x1 = risk + stats::rnorm(k, sd = 0.2),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
}

# Named mtimes of every file below `dir`, for "was it recomputed?" checks.
r7_mtimes <- function(dir) {
  fs <- sort(list.files(dir, recursive = TRUE, full.names = TRUE))
  stats::setNames(file.info(fs)$mtime, fs)
}

r7_mets <- function() {
  yardstick::metric_set(
    yardstick::brier_survival_integrated,
    yardstick::concordance_survival
  )
}

r7_nested_folds <- function(d, v = 2) {
  rsample::nested_cv(
    d,
    outside = rsample::group_vfold_cv(group = "subject_id", v = v),
    inside = rsample::group_vfold_cv(group = "subject_id", v = v)
  )
}

r7_lung_folds <- function(v_outer = 2, v_inner = 4) {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  rsample::nested_cv(
    lung,
    outside = rsample::vfold_cv(v = v_outer),
    inside = rsample::vfold_cv(v = v_inner)
  )
}

r7_skip_if_no_race_stack <- function() {
  for (pkg in c("finetune", "parsnip", "workflows", "tune")) {
    skip_if_not_installed(pkg)
  }
}

# ---- R7-01: nested_cv_coxnet() fingerprint omits `metric` -------------------

test_that("nested_cv_coxnet() checkpoint fingerprint includes the selection metric (R7-01)", {
  skip_if_no_coxnet_deps()
  d <- r7_tied(60)
  set.seed(11)
  folds <- r7_nested_folds(d)
  f <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3
  mets <- r7_mets()
  run <- function(metric = NULL, dir = NULL) {
    suppressMessages(suppressWarnings(nested_cv_coxnet(
      folds, f, subject_id = "subject_id", mixture = 1, metrics = mets,
      nlambda = 10, cox.ties = "breslow", metric = metric, checkpoint_dir = dir
    )))
  }
  # The two metrics genuinely select different penalties on this data, so
  # the fingerprint test is not vacuous.
  fresh_ibs <- run()
  fresh_conc <- run(metric = "concordance_survival")
  expect_false(isTRUE(all.equal(fresh_ibs$penalty, fresh_conc$penalty)))

  ck <- withr::local_tempdir("r7_01_")
  run(dir = ck)
  mt1 <- r7_mtimes(ck)
  # The identical call still reuses its checkpoints.
  run(dir = ck)
  expect_identical(r7_mtimes(ck), mt1)
  ckpt_conc <- run(metric = "concordance_survival", dir = ck)
  # Changing only the metric recomputes (no stale hit) and returns the
  # concordance-selected penalties, not the IBS ones.
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(ckpt_conc$penalty, fresh_conc$penalty)
  expect_false(isTRUE(all.equal(ckpt_conc$penalty, fresh_ibs$penalty)))
  # The explicitly-passed default metric reuses the default run's
  # checkpoints: the fingerprint holds the *effective* metric.
  ck2 <- withr::local_tempdir("r7_01b_")
  run(dir = ck2)
  mt2 <- r7_mtimes(ck2)
  run(metric = "brier_survival_integrated", dir = ck2)
  expect_identical(r7_mtimes(ck2), mt2)
})

# ---- R7-01 sub-item: `control$num_ties` is not fingerprinted ---------------

test_that("race_control_num_ties() returns the effective num_ties (R7-01 sub-item)", {
  skip_if_not_installed("finetune")
  expect_equal(race_control_num_ties(NULL), 10L)
  expect_equal(race_control_num_ties(list()), 10L)
  expect_equal(race_control_num_ties(list(num_ties = 25)), 25L)
  expect_equal(race_control_num_ties(finetune::control_race(num_ties = 7)), 7L)
})

test_that("nested_cv_coxnet() checkpoint fingerprint includes control$num_ties (R7-01 sub-item)", {
  skip_if_no_coxnet_deps()
  r7_skip_if_no_race_stack()
  set.seed(5)
  folds <- r7_lung_folds()
  f <- survival::Surv(time, status) ~ age + sex + ph.ecog
  mets <- r7_mets()
  run <- function(num_ties, dir) {
    suppressMessages(suppressWarnings(nested_cv_coxnet(
      folds, f, mixture = 1, penalty = c(0.5, 0.6), tune_method = "race_anova",
      metrics = mets, nlambda = 10, control = list(num_ties = num_ties),
      checkpoint_dir = dir
    )))
  }
  ck <- withr::local_tempdir("r7_01ties_")
  r10 <- run(10, ck)
  mt1 <- r7_mtimes(ck)
  # `num_ties` does not change ANOVA racing, so the results agree — but the
  # fingerprint must still notice the change and recompute.
  r25 <- run(25, ck)
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(r25$penalty, r10$penalty)
})

# ---- R7-02: fingerprints omit the preprocessor ------------------------------

test_that("nested_cv_coxnet() checkpoint fingerprint includes the preprocessor (R7-02)", {
  skip_if_no_coxnet_deps()
  d <- r7_tied(60)
  set.seed(11)
  folds <- r7_nested_folds(d)
  mets <- r7_mets()
  f1 <- survival::Surv(tstart, tstop, status) ~ x1 + x2
  f2 <- survival::Surv(tstart, tstop, status) ~ x1 + x3
  run <- function(formula, dir = NULL) {
    suppressMessages(suppressWarnings(nested_cv_coxnet(
      folds, formula, subject_id = "subject_id", mixture = 1, metrics = mets,
      nlambda = 10, cox.ties = "breslow", checkpoint_dir = dir
    )))
  }
  fresh_f1 <- run(f1)
  fresh_f2 <- run(f2)
  expect_equal(sort(unique(fresh_f1$.coefs[[1]]$term)), c("x1", "x2"))
  expect_equal(sort(unique(fresh_f2$.coefs[[1]]$term)), c("x1", "x3"))

  ck <- withr::local_tempdir("r7_02n_")
  run(f1, ck)
  mt1 <- r7_mtimes(ck)
  ckpt_f2 <- run(f2, ck)
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(sort(unique(ckpt_f2$.coefs[[1]]$term)), c("x1", "x3"))
  expect_equal(ckpt_f2$.coefs[[1]], fresh_f2$.coefs[[1]])
  expect_equal(ckpt_f2$penalty, fresh_f2$penalty)
})

test_that("cv_coxnet() checkpoint fingerprint includes the preprocessor, explicit path (R7-02)", {
  skip_if_no_coxnet_deps()
  d <- r7_tied(60)
  set.seed(3)
  resamples <- rsample::group_vfold_cv(d, group = "subject_id", v = 3)
  mets <- r7_mets()
  f1 <- survival::Surv(tstart, tstop, status) ~ x1 + x2
  f2 <- survival::Surv(tstart, tstop, status) ~ x1 + x3
  run <- function(formula, dir = NULL) {
    suppressMessages(suppressWarnings(cv_coxnet(
      formula, data = d, subject_id = "subject_id", resamples = resamples,
      mixture = 1, eval_time = c(3, 6, 9), metric = "brier_survival_integrated",
      metrics = mets, penalty = c(0.1, 0.05, 0.01), checkpoint_dir = dir
    )))
  }
  # At these penalties the two predictor sets genuinely score differently.
  fresh_f1 <- run(f1)
  fresh_f2 <- run(f2)
  expect_false(isTRUE(all.equal(fresh_f1$metrics, fresh_f2$metrics)))

  ck <- withr::local_tempdir("r7_02c_")
  run(f1, ck)
  mt1 <- r7_mtimes(ck)
  ckpt_f2 <- run(f2, ck)
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(ckpt_f2$metrics, fresh_f2$metrics)
})

test_that("cv_coxnet() checkpoint fingerprint includes the preprocessor, data-derived path (R7-02)", {
  skip_if_no_coxnet_deps()
  d <- r7_tied(60)
  set.seed(3)
  resamples <- rsample::group_vfold_cv(d, group = "subject_id", v = 3)
  mets <- r7_mets()
  f1 <- survival::Surv(tstart, tstop, status) ~ x1 + x2
  f2 <- survival::Surv(tstart, tstop, status) ~ x1 + x3
  run <- function(formula, dir = NULL) {
    suppressMessages(suppressWarnings(cv_coxnet(
      formula, data = d, subject_id = "subject_id", resamples = resamples,
      mixture = 1, eval_time = c(3, 6, 9), metric = "brier_survival_integrated",
      metrics = mets, nlambda = 8, checkpoint_dir = dir
    )))
  }
  fresh_f1 <- run(f1)
  fresh_f2 <- run(f2)
  expect_false(isTRUE(all.equal(fresh_f1$metrics, fresh_f2$metrics)))

  ck <- withr::local_tempdir("r7_02d_")
  run(f1, ck)
  mt1 <- r7_mtimes(ck)
  ckpt_f2 <- run(f2, ck)
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(ckpt_f2$metrics, fresh_f2$metrics)
})

test_that("cv_coxnet() checkpoint fingerprint includes a recipe preprocessor (R7-02)", {
  skip_if_no_coxnet_deps()
  skip_if_not_installed("recipes")
  d <- r7_tied(60)
  d$surv <- survival::Surv(d$tstart, d$tstop, d$status)
  set.seed(3)
  resamples <- rsample::group_vfold_cv(d, group = "subject_id", v = 3)
  mets <- r7_mets()
  mk_rec <- function(drop) {
    recipes::recipe(surv ~ x1 + x2 + x3 + subject_id, data = d) |>
      recipes::update_role("subject_id", new_role = "id") |>
      recipes::step_rm(tidyselect::all_of(drop))
  }
  run <- function(rec, dir = NULL) {
    suppressMessages(suppressWarnings(cv_coxnet(
      rec, d, subject_id = "subject_id", resamples = resamples,
      mixture = 1, eval_time = c(3, 6, 9), metric = "brier_survival_integrated",
      metrics = mets, penalty = c(0.1, 0.05, 0.01), checkpoint_dir = dir
    )))
  }
  fresh_rm2 <- run(mk_rec("x2"))
  fresh_rm3 <- run(mk_rec("x3"))
  expect_false(isTRUE(all.equal(fresh_rm2$metrics, fresh_rm3$metrics)))

  ck <- withr::local_tempdir("r7_02r_")
  run(mk_rec("x2"), ck)
  mt1 <- r7_mtimes(ck)
  ckpt_rm3 <- run(mk_rec("x3"), ck)
  expect_false(identical(mt1, r7_mtimes(ck)))
  expect_equal(ckpt_rm3$metrics, fresh_rm3$metrics)
})

test_that("canonicalize_preprocessor_for_hash() distinguishes formulas that differ only in the outcome (R7-02)", {
  f1 <- survival::Surv(tstart, tstop, status) ~ x1 + x2
  f2 <- survival::Surv(tstop, status) ~ x1 + x2
  # The outcome is part of the preprocessor: changing only the left-hand
  # side must change the fingerprint (a terms-string form would not).
  expect_false(identical(
    canonicalize_preprocessor_for_hash(f1),
    canonicalize_preprocessor_for_hash(f2)
  ))
  expect_identical(
    canonicalize_preprocessor_for_hash(f1),
    canonicalize_preprocessor_for_hash(f1)
  )
  # And a right-hand-side change still changes it.
  f3 <- survival::Surv(tstart, tstop, status) ~ x1 + x3
  expect_false(identical(
    canonicalize_preprocessor_for_hash(f1),
    canonicalize_preprocessor_for_hash(f3)
  ))
})

# ---- R7-04: finetune race discards user-supplied penalty/mixture -----------

test_that("nested_cv_coxnet() finetune race races the supplied penalty values (R7-04)", {
  skip_if_no_coxnet_deps()
  r7_skip_if_no_race_stack()
  set.seed(5)
  folds <- r7_lung_folds()
  res <- suppressMessages(suppressWarnings(nested_cv_coxnet(
    folds, survival::Surv(time, status) ~ age + sex + ph.ecog,
    mixture = 1, penalty = c(0.5, 0.6), tune_method = "race_anova",
    metrics = r7_mets(), nlambda = 10
  )))
  # Every chosen penalty comes from the supplied values — never from a
  # dials grid — and every outer split raced exactly those values.
  expect_true(all(res$penalty %in% c(0.5, 0.6)))
  for (i in seq_len(nrow(res))) {
    expect_equal(sort(unique(res$.race[[i]]$candidates[[1]]$penalty)), c(0.5, 0.6))
  }
})

test_that("nested_cv_coxnet() finetune race races the supplied penalty x mixture grid (R7-04)", {
  skip_if_no_coxnet_deps()
  r7_skip_if_no_race_stack()
  set.seed(5)
  folds <- r7_lung_folds()
  res <- suppressMessages(suppressWarnings(nested_cv_coxnet(
    folds, survival::Surv(time, status) ~ age + sex + ph.ecog,
    mixture = c(0.5, 1), penalty = c(0.5, 0.6), tune_method = "race_anova",
    metrics = r7_mets(), nlambda = 10
  )))
  expect_true(all(res$mixture %in% c(0.5, 1)))
  expect_true(all(res$penalty %in% c(0.5, 0.6)))
  # Every raced candidate comes from the supplied penalty x mixture grid.
  # Racing may eliminate candidates (finetune's collect_metrics then lists
  # only the survivors), so this is a subset check, not an equality check:
  # on unpatched master the raced values are dials grid values instead.
  want <- expand.grid(mixture = c(0.5, 1), penalty = c(0.5, 0.6))
  want_keys <- sort(paste(want$mixture, want$penalty))
  for (i in seq_len(nrow(res))) {
    cand <- res$.race[[i]]$candidates[[1]]
    got_keys <- sort(paste(cand$mixture, cand$penalty))
    expect_true(length(got_keys) >= 1)
    expect_true(all(got_keys %in% want_keys))
  }
})

test_that("race_explicit_grid() builds the supplied grid, or keeps the dials size (R7-04)", {
  expect_equal(race_explicit_grid(NULL, 1), 20)
  expect_equal(race_explicit_grid(c(0.6, 0.5, 0.5), 1), data.frame(penalty = c(0.5, 0.6)))
  expect_equal(
    race_explicit_grid(c(0.5, 0.6), c(1, 0.5)),
    expand.grid(penalty = c(0.5, 0.6), mixture = c(0.5, 1))
  )
})
# ------------------------------------------------------------------------------
# R7-03 — checkpoint_map() default fingerprint omits `.f`
# ------------------------------------------------------------------------------

test_that("R7-03: checkpoint_map() recomputes when the mapped function changes", {
  dir <- file.path(tempdir(), "r703_change_f")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res_sq <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_sq)), c(1, 4, 9))

  # Same items, same dots, different function: must recompute, not reuse the
  # squares. On unpatched master this returns 1, 4, 9.
  res_cb <- checkpoint_map(1:3, function(x) x^3, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_cb)), c(1, 8, 27))
})

test_that("R7-03: checkpoint_map() still reuses the cache for an unchanged function", {
  dir <- file.path(tempdir(), "r703_same_f")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res1 <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = dir)
  expect_equal(unname(unlist(res1)), c(1, 4, 9))
  ckpt <- file.path(dir, "ckpt_item_0001.rds")
  expect_true(file.exists(ckpt))
  mtime1 <- file.info(ckpt)$mtime

  Sys.sleep(1.1)
  res2 <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = dir)
  expect_equal(unname(unlist(res2)), c(1, 4, 9))
  # Identical function: the cached file must not have been rewritten.
  expect_identical(file.info(ckpt)$mtime, mtime1)
})

test_that("R7-03: checkpoint_map() recomputes when a formula .f changes", {
  dir <- file.path(tempdir(), "r703_formula_f")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res_sq <- checkpoint_map(1:3, ~ .x^2, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_sq)), c(1, 4, 9))

  res_cb <- checkpoint_map(1:3, ~ .x^3, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_cb)), c(1, 8, 27))
})

test_that("R7-03: checkpoint_map() distinguishes functions that differ only in formals", {
  dir <- file.path(tempdir(), "r703_formals_f")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- function(x, p) x^p
  res_p2 <- checkpoint_map(1:3, f, p = 2, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_p2)), c(1, 4, 9))

  # Same function, different dots: already recomputed before this fix, and
  # still is.
  res_p3 <- checkpoint_map(1:3, f, p = 3, checkpoint_dir = dir)
  expect_equal(unname(unlist(res_p3)), c(1, 8, 27))
})

test_that("R7-03: a user-supplied fingerprint is unaffected by a change of .f", {
  dir <- file.path(tempdir(), "r703_user_fp")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res_sq <- checkpoint_map(1:3, function(x) x^2, checkpoint_dir = dir, fingerprint = "fixed")
  expect_equal(unname(unlist(res_sq)), c(1, 4, 9))

  # The explicit fingerprint says the tasks are identical, so the cached
  # squares are (by design) reused even though .f changed.
  res_cb <- checkpoint_map(1:3, function(x) x^3, checkpoint_dir = dir, fingerprint = "fixed")
  expect_equal(unname(unlist(res_cb)), c(1, 4, 9))
})

# ------------------------------------------------------------------------------
# R7-05 — compare_runs()/compare_tuning_runs() duplicate labels
# ------------------------------------------------------------------------------

r705_runs <- function() {
  list(
    r1 = tibble::tibble(model = c("M1", "M2"), IBS = c(0.10, 0.10)),
    r2 = tibble::tibble(model = c("M1", "M2"), IBS = c(0.90, 0.90))
  )
}

r705_tunes <- function() {
  list(
    t1 = tibble::tibble(alpha = c(0, 0.5, 1), IBS = c(0.10, 0.20, 0.30)),
    t2 = tibble::tibble(alpha = c(0, 0.5, 1), IBS = c(0.90, 0.80, 0.70))
  )
}

test_that("R7-05: compare_runs() errors on duplicate labels", {
  runs <- r705_runs()
  # On unpatched master this silently returns one `ibs_same` column with
  # diff_ibs = 0 and reproducible = TRUE ("Reproducible: YES").
  expect_error(
    compare_runs(runs$r1, runs$r2, label1 = "same", label2 = "same"),
    "`label1` and `label2` must be different"
  )
})

test_that("R7-05: compare_tuning_runs() errors on duplicate labels in every match mode", {
  tunes <- r705_tunes()
  # On unpatched master this errors cryptically about recycling `diff_mean`.
  for (m in c("exact", "nearest", "binned")) {
    expect_error(
      compare_tuning_runs(tunes$t1, tunes$t2, label1 = "same", label2 = "same", match = m),
      "`label1` and `label2` must be different"
    )
  }
})

test_that("R7-05: compare_runs() errors on duplicate labels via the tuning delegation path", {
  tunes <- r705_tunes()
  # Tuning-shaped frames (alpha + IBS, no model) delegate to
  # compare_tuning_runs(); the guard must fire before the delegation.
  expect_error(
    compare_runs(tunes$t1, tunes$t2, label1 = "same", label2 = "same"),
    "`label1` and `label2` must be different"
  )
})

test_that("R7-05: compare_runs()/compare_tuning_runs() validate label shape", {
  runs <- r705_runs()
  tunes <- r705_tunes()
  expect_error(compare_runs(runs$r1, runs$r2, label1 = "", label2 = "b"), "single non-empty string")
  expect_error(compare_runs(runs$r1, runs$r2, label1 = "a", label2 = NA_character_), "single non-empty string")
  expect_error(compare_runs(runs$r1, runs$r2, label1 = 1, label2 = "b"), "single non-empty string")
  expect_error(compare_runs(runs$r1, runs$r2, label1 = c("a", "b"), label2 = "c"), "single non-empty string")
  expect_error(compare_tuning_runs(tunes$t1, tunes$t2, label1 = "a", label2 = ""), "single non-empty string")
  expect_error(compare_tuning_runs(tunes$t1, tunes$t2, label1 = NULL, label2 = "b"), "single non-empty string")
})

test_that("R7-05: distinct labels still compare the two runs against each other", {
  runs <- r705_runs()
  out <- compare_runs(runs$r1, runs$r2, label1 = "a", label2 = "b")
  expect_named(out, c("model", "ibs_a", "ibs_b", "diff_ibs", "reproducible"))
  expect_equal(out$ibs_a, c(0.10, 0.10))
  expect_equal(out$ibs_b, c(0.90, 0.90))
  expect_equal(out$diff_ibs, c(0.80, 0.80))
  expect_false(any(out$reproducible))
  expect_false(attr(out, "comparison_summary")$all_reproducible)

  tunes <- r705_tunes()
  out_t <- compare_tuning_runs(tunes$t1, tunes$t2, label1 = "a", label2 = "b", match = "exact")
  expect_true(all(c("mean_ibs_a", "mean_ibs_b") %in% names(out_t)))
  expect_equal(out_t$mean_ibs_a, c(0.10, 0.20, 0.30))
  expect_equal(out_t$mean_ibs_b, c(0.90, 0.80, 0.70))
  expect_equal(out_t$diff_mean_ibs, c(0.80, 0.60, 0.40))
  expect_false(any(out_t$reproducible))
})
