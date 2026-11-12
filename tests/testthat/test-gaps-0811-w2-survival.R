# Tests for the Part 2 survival feature drafts (A1, B1, B7) of
# TempleCBE-gaps-and-features-2026-11-11.md.

# Recurrent data: 1-3 intervals per subject, with events in any interval
# (not only the last one), so the data are genuinely recurrent.
sim_recurrent <- function(n = 60, seed = 1) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n), function(i) {
    k <- sample(1:3, 1)
    stops <- round(cumsum(stats::runif(k, 2, 6)), 2)
    risk <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, utils::head(stops, -1)), tstop = stops,
      status = stats::rbinom(k, 1, stats::plogis(1.5 * risk - 1)),
      x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k)
    )
  }))
}

# Readmission intervals with deaths at varying times: a subject who dies has
# their intervals cut at the death time, with death marked on the last one.
sim_readmission_death <- function(n = 120, seed = 1) {
  set.seed(seed)
  subj <- data.frame(id = seq_len(n), age = stats::rnorm(n), end = 120)
  ev <- do.call(rbind, lapply(seq_len(n), function(i) {
    k <- stats::rpois(1, exp(0.3 * subj$age[i]) * 1.2)
    if (k == 0) return(NULL)
    data.frame(id = i, time = sort(stats::runif(k, 1, 120)))
  }))
  long <- recurrent_intervals(ev, subj)
  long$age <- subj$age[long$id]
  long$death <- 0L
  died <- stats::rbinom(n, 1, stats::plogis(-1 + 0.5 * subj$age)) == 1
  death_time <- stats::runif(n, 20, 120)
  for (i in which(died)) {
    rows_i <- which(long$id == i)
    cut <- truncate_start_stop(long[rows_i, , drop = FALSE], death_time[i])
    long <- long[-rows_i, , drop = FALSE]
    cut$death <- 0L
    cut$death[nrow(cut)] <- 1L
    long <- rbind(long, cut)
  }
  long[order(long$id, long$tstart), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# A1: recurrent-event scoring by gap-time concordance
# ---------------------------------------------------------------------------

test_that("surv_gap_truth returns one row per gap with gap times (A1)", {
  skip_if_not_installed("survival")
  long <- data.frame(
    subject_id = c(1, 1, 2, 3, 3),
    tstart = c(0, 5, 0, 0, 4),
    tstop = c(5, 9, 6, 4, 10),
    status = c(1, 1, 0, 0, 0)
  )
  out <- surv_gap_truth(
    survival::Surv(long$tstart, long$tstop, long$status),
    subject_id = long$subject_id
  )
  expect_named(out, c(".subject_id", ".gap", ".entry", ".truth"))
  expect_equal(out$.subject_id, c(1, 1, 2, 3, 3))
  expect_equal(out$.gap, c(1L, 2L, 1L, 1L, 2L))
  expect_equal(unname(out$.truth[, "time"]), c(5, 4, 6, 4, 6))
  expect_equal(unname(out$.truth[, "status"]), c(1, 1, 0, 0, 0))
  expect_error(
    surv_gap_truth(survival::Surv(long$tstart, long$tstop, long$status)),
    "subject_id"
  )
  # overlapping intervals still error
  bad <- long
  bad$tstart[2] <- 4
  expect_error(
    surv_gap_truth(survival::Surv(bad$tstart, bad$tstop, bad$status), subject_id = bad$subject_id),
    "overlap"
  )
})

test_that("gap-time concordance orders gaps by risk, and is NA with no events (A1)", {
  skip_if_not_installed("survival")
  skip_if_not_installed("yardstick")
  long <- sim_recurrent(n = 40, seed = 2)
  g <- surv_gap_truth(
    survival::Surv(long$tstart, long$tstop, long$status),
    subject_id = long$subject_id
  )
  # A perfect risk score (shorter gaps = higher risk) scores above 0.9;
  # a reversed one scores below 0.1.
  gap_time <- unname(g$.truth[, "time"])
  expect_gt(gap_time_concordance(g, -gap_time), 0.9)
  expect_lt(gap_time_concordance(g, gap_time), 0.1)
  # no events: no comparable pair
  g0 <- surv_gap_truth(
    survival::Surv(c(0, 0, 0), c(5, 6, 7), c(0, 0, 0)),
    subject_id = c(1, 2, 3)
  )
  expect_true(is.na(gap_time_concordance(g0, c(0.1, 0.2, 0.3))))
})

test_that("cv_coxnet(recurrent = 'gap') scores recurrent data on cgd (A1)", {
  skip_if_no_coxnet_deps()
  data(cgd, package = "survival")
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ treat + age + sex,
    data = cgd, subject_id = "id",
    mixture = 1, v = 3, nlambda = 10, recurrent = "gap",
    cox.ties = "breslow", seed = 1
  )
  expect_s3_class(cv, "cv_coxnet")
  expect_equal(cv$metric, "concordance_survival")
  expect_equal(cv$recurrent, "gap")
  expect_true(all(cv$fold_metrics$.metric == "concordance_survival"))
  expect_true(all(is.na(cv$fold_metrics$.eval_time)))
  # The consolidated benchmark's manual gap-time CV gave C ~ 0.60 on cgd;
  # the penalized fit should be in the same region, not at chance or perfect.
  expect_gt(cv$best$mean, 0.55)
  expect_lt(cv$best$mean, 0.85)
  # the default still errors on recurrent data, and names the gap option
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ treat + age,
      data = cgd, subject_id = "id",
      mixture = 1, v = 3, nlambda = 10, cox.ties = "breslow", seed = 1
    ),
    "gap"
  )
  # a non-concordance metric set is an error in gap mode
  expect_error(
    cv_coxnet(
      survival::Surv(tstart, tstop, status) ~ treat + age,
      data = cgd, subject_id = "id",
      mixture = 1, v = 3, nlambda = 10, recurrent = "gap",
      metrics = yardstick::metric_set(yardstick::brier_survival_integrated),
      cox.ties = "breslow", seed = 1
    ),
    "concordance"
  )
})

test_that("cv_coxnet(recurrent = 'first') still scores time to first event (A1)", {
  skip_if_no_coxnet_deps()
  long <- sim_recurrent(n = 60, seed = 1)
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    data = long, subject_id = "subject_id",
    mixture = 1, v = 3, nlambda = 10, recurrent = "first",
    cox.ties = "breslow", seed = 1
  )
  expect_equal(cv$metric, "brier_survival_integrated")
  expect_equal(cv$recurrent, "first")
  expect_true(is.finite(cv$best$mean))
})

test_that("glmnet_IBS(recurrent = 'gap') reports gap concordance, not IBS (A1)", {
  skip_if_no_coxnet_deps()
  long <- sim_recurrent(n = 60, seed = 1)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable") |>
    recipes::step_range(recipes::all_numeric_predictors())
  split <- rsample::group_initial_split(long, group = subject_id)
  res <- glmnet_IBS(
    split, alpha = 1, recipe = rec, feature_names = c("x1", "x2"),
    time_data = unique(long[, c("tstart", "tstop")]), internal_folds = 3,
    id_col = "subject_id", cox.ties = "breslow", recurrent = "gap"
  )
  expect_true(all(is.na(res$IBS)))
  expect_true("gap_concordance" %in% names(res))
  expect_true(all(res$gap_concordance > 0.4 & res$gap_concordance < 0.95))
  expect_equal(res$gap_concordance[1], res$gap_concordance[2])
  # a non-concordance metric is an error, not a silent failure row
  expect_error(
    glmnet_IBS(
      split, alpha = 1, recipe = rec, feature_names = c("x1", "x2"),
      time_data = unique(long[, c("tstart", "tstop")]), internal_folds = 3,
      id_col = "subject_id", cox.ties = "breslow", recurrent = "gap",
      metric = "brier_survival_integrated"
    ),
    "concordance"
  )
})

test_that("score_outer(recurrent = 'gap') carries gap concordance (A1)", {
  skip_if_no_coxnet_deps()
  long <- sim_recurrent(n = 60, seed = 1)
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable") |>
    recipes::step_range(recipes::all_numeric_predictors())
  split <- rsample::group_initial_split(long, group = subject_id)
  so <- score_outer(
    split, grid = data.frame(alpha = c(0.5, 1)), recipe = rec,
    feature_names = c("x1", "x2"),
    time_data = unique(long[, c("tstart", "tstop")]), internal_folds = 3,
    id_col = "subject_id", cox.ties = "breslow", recurrent = "gap"
  )
  expect_equal(nrow(so), 2L)
  expect_true(all(is.na(so$IBS)))
  expect_true(all(so$gap_concordance > 0.4 & so$gap_concordance < 0.95))
})

test_that("nested_cv_coxnet(recurrent = 'gap') scores outer gaps (A1)", {
  skip_if_no_coxnet_deps()
  long <- sim_recurrent(n = 60, seed = 1)
  folds <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 2),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  res <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2,
    subject_id = "subject_id", mixture = 1, recurrent = "gap",
    nlambda = 10, cox.ties = "breslow"
  )
  expect_equal(nrow(res), 2L)
  expect_true(all(vapply(res$.metrics, function(m) all(m$.metric == "concordance_survival"), logical(1))))
  expect_true(all(vapply(res$.metrics, function(m) all(m$.estimate > 0.3 & m$.estimate < 0.95), logical(1))))
})

test_that("nested_survival_run(recurrent = 'gap') runs end to end (A1)", {
  skip_if_no_coxnet_deps()
  long <- sim_recurrent(n = 120, seed = 1)
  run <- nested_survival_run(
    long, id = "subject_id", outer_v = 2, inner_v = 2, alphas = c(0.5, 1),
    internal_folds = 2, recurrent = "gap", seed = 1, cox.ties = "breslow"
  )
  expect_s3_class(run, "cbe_nested_survival_run")
  expect_true("gap_concordance" %in% names(run$tune_results))
  expect_true(all(is.na(run$tune_results$IBS)))
  expect_true(all(run$tune_results$gap_concordance > 0.3, na.rm = TRUE))
  expect_true("gap_concordance" %in% names(run$outer_scores))
  expect_true(all(is.na(run$outer_scores$IBS)))
  expect_true(all(run$outer_scores$gap_concordance > 0.3 & run$outer_scores$gap_concordance < 0.95))
  # the censoring-weights comparison is skipped in gap mode
  expect_null(run$censoring_comparison)
  # and the print method reports concordance, not IBS
  expect_output(print(run), "gap concordance")
})

# ---------------------------------------------------------------------------
# B7: R-side time-varying-coefficient Cox model
# ---------------------------------------------------------------------------

test_that("cox_time_varying fits, tidies and prints (B7)", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 200
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  expect_s3_class(fit, "cox_time_varying")
  expect_equal(fit$tv_vars, "age")
  expect_equal(fit$time_fn_label, "log")
  expect_equal(fit$n_events, sum(d$status))
  tab <- generics::tidy(fit)
  expect_named(tab, c("term", "estimate", "std_error", "hr", "conf_low", "conf_high",
                      "statistic", "p_value", "time_varying"))
  expect_equal(tab$term, c("age", "sex", "tt(age)"))
  expect_equal(tab$time_varying, c(FALSE, FALSE, TRUE))
  expect_output(print(fit), "Time-varying-coefficient Cox model")
  expect_output(print(fit), "tt\\(age\\)")
})

test_that("cox_time_varying recovers a known time-varying effect (B7)", {
  skip_if_not_installed("survival")
  # log HR(t) = gamma * x * log(t) with gamma = 0.5, simulated exactly.
  set.seed(7)
  n <- 400
  gamma_true <- 0.5
  h0 <- 0.02
  x <- stats::rbinom(n, 1, 0.5)
  u <- stats::runif(n)
  t <- (-(gamma_true * x + 1) * log(u) / h0)^(1 / (gamma_true * x + 1))
  d <- data.frame(
    time = pmin(t, 100), status = as.integer(t <= 100),
    x = x, z = stats::rnorm(n)
  )
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ x + z, tv_vars = "x")
  expect_equal(unname(stats::coef(fit$fit)["tt(x)"]), 0.5, tolerance = 0.15)
  expect_lt(abs(unname(stats::coef(fit$fit)["x"])), 0.15)
  # the hazard ratio grows with time: ~1 at t = 1, ~exp(0.5 * log 10) at t = 10
  hr <- hazard_ratio_at_time(fit, "x", times = c(1, 10, 50))
  expect_equal(hr$hazard_ratio[1], 1, tolerance = 0.2)
  expect_equal(hr$hazard_ratio[2], exp(0.5 * log(10)), tolerance = 0.2)
  expect_gt(hr$hazard_ratio[3], hr$hazard_ratio[2])
  expect_true(all(hr$conf_low < hr$hazard_ratio & hr$hazard_ratio < hr$conf_high))
})

test_that("cox_time_varying predict matches coef arithmetic and is monotone (B7)", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 200
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  b <- stats::coef(fit$fit)
  lp10 <- predict(fit, d[1:3, ], type = "linear_pred", eval_time = 10)
  man10 <- b["age"] * d$age[1] + b["sex"] * d$sex[1] + b["tt(age)"] * d$age[1] * log(10)
  expect_equal(lp10$.pred_linear_pred[1], unname(man10))
  # the linear predictor changes with the evaluation time
  lp100 <- predict(fit, d[1:3, ], type = "linear_pred", eval_time = 100)
  expect_false(isTRUE(all.equal(lp10$.pred_linear_pred, lp100$.pred_linear_pred)))
  sv <- predict(fit, d[1:3, ], type = "survival", eval_time = c(10, 50, 100))
  expect_equal(nrow(sv), 3L)
  expect_true(all(vapply(sv$.pred, function(p) all(diff(p$.pred_survival) <= 0), logical(1))))
  expect_true(all(vapply(sv$.pred, function(p) all(p$.pred_survival >= 0 & p$.pred_survival <= 1), logical(1))))
  # with a near-zero time-varying effect, survival is close to a plain Cox fit
  fit0 <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  sf0 <- survival::survfit(fit0, newdata = d[1, ])
  expect_equal(
    sv$.pred[[1]]$.pred_survival[2],
    unname(summary(sf0, times = 50)$surv),
    tolerance = 0.05
  )
  expect_error(predict(fit, d[1:3, ], type = "linear_pred"), "eval_time")
  expect_error(predict(fit, d[1:3, ], type = "survival"), "eval_time")
})

test_that("cox_time_varying rejects bad tv_vars and factors (B7)", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 100
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), grp = factor(sample(c("a", "b"), n, replace = TRUE))
  )
  expect_error(
    cox_time_varying(d, survival::Surv(time, status) ~ age + grp, tv_vars = "zzz"),
    "must name terms"
  )
  expect_error(
    cox_time_varying(d, survival::Surv(time, status) ~ age + grp, tv_vars = "grp"),
    "must be numeric"
  )
  expect_error(
    cox_time_varying(d, survival::Surv(time, status) ~ age + grp, tv_vars = character()),
    "non-empty"
  )
  # a non-tv factor predictor is fine
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + grp, tv_vars = "age")
  expect_true("grpb" %in% generics::tidy(fit)$term)
  # time_fn = "identity" and a user function both fit
  expect_s3_class(
    cox_time_varying(d, survival::Surv(time, status) ~ age, tv_vars = "age", time_fn = "identity"),
    "cox_time_varying"
  )
  expect_s3_class(
    cox_time_varying(d, survival::Surv(time, status) ~ age, tv_vars = "age", time_fn = sqrt),
    "cox_time_varying"
  )
})

# ---------------------------------------------------------------------------
# B1: joint model of readmission and death (draft)
# ---------------------------------------------------------------------------

test_that("joint_readmission_death fits both processes and tidies (B1)", {
  skip_if_not_installed("survival")
  long <- sim_readmission_death(n = 120, seed = 1)
  fit <- joint_readmission_death(long, ~ age, ~ age)
  expect_s3_class(fit, "joint_readmission_death")
  expect_equal(fit$n_subjects, 120L)
  expect_gt(fit$n_readmissions, 0)
  expect_gt(fit$n_deaths, 0)
  expect_false(fit$frailty)
  expect_true(all(is.na(fit$frailty_variance)))
  tab <- generics::tidy(fit)
  expect_named(tab, c("process", "term", "estimate", "std_error", "hr", "conf_low",
                      "conf_high", "statistic", "p_value"))
  expect_equal(sort(unique(tab$process)), c("death", "readmission"))
  expect_true("n_prior_readmissions" %in% tab$term[tab$process == "death"])
  expect_output(print(fit), "Joint model of readmission and death")
  expect_output(print(fit), "DRAFT")
  # the readmission effect is recovered: age drives readmissions in the sim
  expect_gt(tab$hr[tab$process == "readmission" & tab$term == "age"], 1)
})

test_that("joint_readmission_death predictions are ordered and monotone (B1)", {
  skip_if_not_installed("survival")
  long <- sim_readmission_death(n = 120, seed = 1)
  fit <- joint_readmission_death(long, ~ age, ~ age)
  nd <- data.frame(age = c(-1, 0, 1))
  sv <- predict(fit, nd, type = "death_survival", eval_time = c(30, 60, 120))
  expect_equal(nrow(sv), 3L)
  expect_true(all(vapply(sv$.pred, function(p) all(diff(p$.pred_survival) <= 0), logical(1))))
  expect_true(all(vapply(sv$.pred, function(p) all(p$.pred_survival >= 0 & p$.pred_survival <= 1), logical(1))))
  # older subjects die sooner
  expect_gt(sv$.pred[[1]]$.pred_survival[3], sv$.pred[[3]]$.pred_survival[3])
  # more prior readmissions lower death survival
  sv2 <- predict(fit, nd, type = "death_survival", eval_time = 120, n_prior_readmissions = 2)
  expect_lt(sv2$.pred[[2]]$.pred_survival, sv$.pred[[2]]$.pred_survival[3])
  cr <- predict(fit, nd, type = "readmission_cumhaz", eval_time = c(30, 60, 120))
  expect_true(all(vapply(cr$.pred, function(p) all(diff(p$.pred_cum_readmissions) >= 0), logical(1))))
  # older subjects accumulate more readmissions
  expect_lt(cr$.pred[[1]]$.pred_cum_readmissions[3], cr$.pred[[3]]$.pred_cum_readmissions[3])
  expect_error(predict(fit, nd, type = "death_survival"), "eval_time")
})

test_that("joint_readmission_death with frailty reports both variances (B1)", {
  skip_if_not_installed("survival")
  long <- sim_readmission_death(n = 120, seed = 1)
  fit <- joint_readmission_death(long, ~ age, ~ age, frailty = TRUE)
  expect_true(fit$frailty)
  expect_equal(names(fit$frailty_variance), c("readmission", "death"))
  expect_true(all(is.finite(fit$frailty_variance)))
  expect_true(all(fit$frailty_variance >= 0))
  nd <- data.frame(age = 0)
  sv <- predict(fit, nd, type = "death_survival", eval_time = c(30, 120))
  expect_true(all(sv$.pred[[1]]$.pred_survival >= 0 & sv$.pred[[1]]$.pred_survival <= 1))
})

test_that("joint_readmission_death validates the death column (B1)", {
  skip_if_not_installed("survival")
  long <- sim_readmission_death(n = 60, seed = 1)
  # death on a non-last interval
  bad <- long
  first_rows <- which(!duplicated(bad$id))
  bad$death[first_rows[1]] <- 1L
  expect_error(joint_readmission_death(bad, ~ age, ~ age), "last interval")
  # a row carrying both a readmission and a death
  bad2 <- long
  last_rows <- which(!duplicated(bad2$id, fromLast = TRUE))
  bad2$status[last_rows[1]] <- 1L
  bad2$death[last_rows[1]] <- 1L
  expect_error(joint_readmission_death(bad2, ~ age, ~ age), "both a readmission and a death")
  # no deaths at all
  nod <- long
  nod$death <- 0L
  expect_error(joint_readmission_death(nod, ~ age, ~ age), "no deaths")
  # no death column
  expect_error(joint_readmission_death(long[, setdiff(names(long), "death")], ~ age, ~ age), "death")
  # death predictors must be numeric in this draft
  long$grp <- factor(sample(c("a", "b"), nrow(long), replace = TRUE))
  expect_error(joint_readmission_death(long, ~ age, ~ grp), "must be numeric")
})
