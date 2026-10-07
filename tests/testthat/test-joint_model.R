test_that("joint_model fits coxnet, status, and time models on 2-parameter data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 50
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, mixture = 1, penalty = 0.05)
  expect_s3_class(fit, "joint_model")
  expect_true(!is.null(fit$coxnet_model))
  expect_true(!is.null(fit$status_model))
  expect_true(!is.null(fit$time_model))

  # Test print
  expect_output(print(fit), "TempleCBE Joint Survival-Status-Time Model")

  # Test predict
  preds <- stats::predict(fit, new_data = df[1:5, ])
  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 5L)
  expect_true(all(c(".pred_survival", ".pred_status", ".pred_status_calibrated",
                    ".pred_time", ".pred_linear_pred", ".pred_risk_score") %in% names(preds)))

  # Test tidy
  td <- generics::tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("term", "estimate_coxnet", "estimate_status", "estimate_time") %in% names(td)))
  expect_equal(td$term, c("x1", "x2"))
})

test_that("joint_model supports counting-process start/stop data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(42)
  t_mid <- stats::runif(30, 2, 5)
  t_end <- t_mid + stats::runif(30, 2, 7)
  long <- data.frame(
    subject_id = rep(1:30, each = 2),
    tstart = as.vector(rbind(rep(0, 30), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, 30), stats::rbinom(30, 1, 0.6))),
    x1 = stats::rnorm(60),
    x2 = stats::rnorm(60)
  )

  fit <- joint_model(
    long,
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    subject_id = "subject_id",
    penalty = 0.05
  )
  expect_s3_class(fit, "joint_model")
  expect_equal(fit$components$type, "counting")
  expect_equal(length(fit$components$start), 60L)
  # `subject_id` must not leak into coxnet()/glmnet() as a stray argument, nor
  # be picked up by `~ .` as a predictor.
  expect_false("subject_id" %in% fit$components$pred_names)

  preds <- stats::predict(fit, new_data = long[1:4, ])
  expect_equal(nrow(preds), 4L)
})

test_that("joint_model auto-tunes penalty for counting-process data with subject_id", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(43)
  t_mid <- stats::runif(30, 2, 5)
  t_end <- t_mid + stats::runif(30, 2, 7)
  long <- data.frame(
    subject_id = rep(1:30, each = 2),
    tstart = as.vector(rbind(rep(0, 30), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, 30), stats::rbinom(30, 1, 0.6))),
    x1 = stats::rnorm(60),
    x2 = stats::rnorm(60)
  )

  fit <- joint_model(
    long,
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    subject_id = "subject_id"
  )
  expect_s3_class(fit, "joint_model")
  expect_s3_class(fit$coxnet_model, "cv_coxnet")
})

test_that("joint_model supports baguette and stacks engines", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("stacks")
  skip_if_not_installed("parsnip")

  set.seed(99)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.5),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  # Baguette engine
  fit_bag <- joint_model(
    df,
    survival::Surv(time, status) ~ x1 + x2,
    engine = "baguette",
    penalty = 0.05
  )
  expect_s3_class(fit_bag, "joint_model")
  expect_s3_class(fit_bag$status_model, "model_fit")
  expect_s3_class(fit_bag$time_model, "model_fit")

  preds_bag <- stats::predict(fit_bag, new_data = df[1:4, ])
  expect_equal(nrow(preds_bag), 4L)

  # Stacks engine (it warns that its main predictions equal "baguette"'s; see the test below)
  expect_warning(
    fit_stack <- joint_model(
      df,
      survival::Surv(time, status) ~ x1 + x2,
      engine = "stacks",
      penalty = 0.05
    ),
    "same bagged trees"
  )
  expect_s3_class(fit_stack, "joint_model")
  expect_true(!is.null(fit_stack$stack_model))
})

test_that("cv_joint_model computes cross-validated IBS and concordance", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(777)
  n <- 45
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  cv_res <- cv_joint_model(
    df,
    survival::Surv(time, status) ~ x1 + x2,
    v = 3,
    penalty = 0.05
  )

  expect_s3_class(cv_res, "cv_joint_model")
  expect_true(all(c("model", "ibs", "concordance", "fold") %in% names(cv_res)))
  expect_setequal(unique(cv_res$model), c("coxnet", "status_calibrated", "time_regression"))
  expect_true(all(is.finite(cv_res$ibs)))

  # Autoplot
  p <- ggplot2::autoplot(cv_res)
  expect_s3_class(p, "ggplot")
})

test_that("cv_joint_model scores counting-process start/stop data one row per subject", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  # predict.joint_model() returns one row per interval for start/stop data;
  # cv_joint_model() must collapse to one row per subject (via
  # surv_subject_truth()) before scoring, instead of crashing or silently
  # scoring at the interval level.
  set.seed(44)
  n_subjects <- 40
  t_mid <- stats::runif(n_subjects, 2, 5)
  t_end <- t_mid + stats::runif(n_subjects, 2, 7)
  long <- data.frame(
    subject_id = rep(seq_len(n_subjects), each = 2),
    tstart = as.vector(rbind(rep(0, n_subjects), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, n_subjects), stats::rbinom(n_subjects, 1, 0.6))),
    x1 = stats::rnorm(2 * n_subjects),
    x2 = stats::rnorm(2 * n_subjects)
  )

  cv_res <- cv_joint_model(
    long,
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    v = 3,
    subject_id = "subject_id",
    penalty = 0.05
  )

  expect_s3_class(cv_res, "cv_joint_model")
  expect_true(all(c("model", "ibs", "concordance", "fold") %in% names(cv_res)))
  expect_setequal(unique(cv_res$model), c("coxnet", "status_calibrated", "time_regression"))
  expect_true(all(is.finite(cv_res$ibs)))
})

test_that("nested_cv_joint_model scores counting-process start/stop data one row per subject", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(45)
  n_subjects <- 40
  t_mid <- stats::runif(n_subjects, 2, 5)
  t_end <- t_mid + stats::runif(n_subjects, 2, 7)
  long <- data.frame(
    subject_id = rep(seq_len(n_subjects), each = 2),
    tstart = as.vector(rbind(rep(0, n_subjects), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, n_subjects), stats::rbinom(n_subjects, 1, 0.6))),
    x1 = stats::rnorm(2 * n_subjects),
    x2 = stats::rnorm(2 * n_subjects)
  )

  folds <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )

  res <- nested_cv_joint_model(
    folds,
    survival::Surv(tstart, tstop, status) ~ x1 + x2,
    subject_id = "subject_id",
    penalty = 0.05
  )

  expect_s3_class(res, "nested_cv_joint_model")
  expect_true(all(c("outer_id", "model", "ibs") %in% names(res)))
  expect_setequal(unique(res$model), c("coxnet", "status_calibrated", "time_regression"))
  expect_true(all(is.finite(res$ibs)))
})

test_that("nested_cv_joint_model scores outer splits across models", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")

  set.seed(42)
  # large enough that every outer analysis set clears glmnet's small-fold /
  # small-class warnings (which would otherwise be noise in the test log)
  n <- 120
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )

  folds <- rsample::nested_cv(
    df,
    outside = rsample::vfold_cv(v = 2),
    inside = rsample::vfold_cv(v = 2)
  )

  expect_no_warning(
    res <- nested_cv_joint_model(
      folds,
      outcome = survival::Surv(time, status) ~ x1 + x2,
      penalty = 0.05
    )
  )

  expect_s3_class(res, "nested_cv_joint_model")
  expect_true(all(c("outer_id", "model", "ibs") %in% names(res)))
  expect_setequal(unique(res$model), c("coxnet", "status_calibrated", "time_regression"))
  expect_equal(nrow(res), 6L)
  expect_setequal(res$outer_id, folds$id)
  # an IBS is a mean squared error of probabilities: finite and within [0, 1]
  expect_true(all(is.finite(res$ibs) & res$ibs >= 0 & res$ibs <= 1))
})

test_that("joint_model keeps subject_id out of the coxnet fit when the formula uses `~ .`", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(46)
  n_subjects <- 30
  t_mid <- stats::runif(n_subjects, 2, 5)
  t_end <- t_mid + stats::runif(n_subjects, 2, 7)
  long <- data.frame(
    subject_id = rep(seq_len(n_subjects), each = 2),
    tstart = as.vector(rbind(rep(0, n_subjects), t_mid)),
    tstop = as.vector(rbind(t_mid, t_end)),
    status = as.vector(rbind(rep(0, n_subjects), stats::rbinom(n_subjects, 1, 0.6))),
    x1 = stats::rnorm(2 * n_subjects),
    x2 = stats::rnorm(2 * n_subjects)
  )

  # With a fixed penalty, coxnet() (which has no `subject_id` argument) is fit on
  # `data` directly; `~ .` would otherwise pick the identifier up as a covariate.
  fit <- joint_model(
    long,
    survival::Surv(tstart, tstop, status) ~ .,
    subject_id = "subject_id",
    penalty = 0.05
  )
  expect_setequal(fit$components$pred_names, c("x1", "x2"))
  expect_setequal(colnames(fit$coxnet_model$x), c("x1", "x2"))
})

test_that("joint_model rejects a subject_id that is not a column of the data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(47)
  df <- data.frame(
    time = stats::rexp(40, rate = 0.1) + 0.1,
    status = stats::rbinom(40, 1, 0.6),
    x1 = stats::rnorm(40)
  )
  expect_error(
    extract_surv_components(df, survival::Surv(time, status) ~ x1, subject_id = "not_a_column"),
    "subject_id"
  )
})

test_that("predict.joint_model re-applies the training encoding to factor predictors", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  set.seed(48)
  n <- 80
  df <- data.frame(
    time = stats::rexp(n, rate = 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    grp = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )

  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + grp, penalty = 0.05)
  # one-hot encoding: one column per level, not treatment contrasts
  expect_setequal(fit$components$pred_names, c("x1", "grpa", "grpb", "grpc"))

  # new data that only exhibits one level of `grp` must still be encoded
  # against all three training levels
  one_level <- df[df$grp == "a", ][1:3, ]
  one_level$grp <- factor(as.character(one_level$grp))
  preds <- stats::predict(fit, new_data = one_level)
  expect_equal(nrow(preds), 3L)
  expect_true(all(is.finite(preds$.pred_time)))
  expect_true(all(is.finite(preds$.pred_status)))
})

# ---------------------------------------------------------------------------
# Review findings A2-01 .. A2-08: subjects, scoring rule, calibrator, ...
# ---------------------------------------------------------------------------

f_long <- survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3

test_that("start/stop data without subject_id is an error in the joint-model cross-validations (A2-01)", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 50, seed = 3)

  # It used to run, scoring every interval as its own subject (and, with the
  # default folds, splitting a subject's intervals between the two sides).
  expect_error(cv_joint_model(long, f_long, v = 3, penalty = 0.05), "subject_id.*required|required.*subject_id")

  nested <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(nested_cv_joint_model(nested, f_long, penalty = 0.05), "subject_id.*required|required.*subject_id")

  idx <- long$subject_id <= 40
  expect_error(
    joint_model_fold_scores(long[idx, ], long[!idx, ], f_long, NULL, "glmnet", FALSE, penalty = 0.05),
    "subject_id.*required|required.*subject_id"
  )

  # right-censored data does not need one: each row is a subject
  df <- long[!duplicated(long$subject_id, fromLast = TRUE), ]
  df$time <- df$tstop
  expect_no_error(
    cv_joint_model(df, survival::Surv(time, status) ~ x1 + x2 + x3, v = 3, penalty = 0.05)
  )
})

test_that("supplied resamples that put a subject on both sides of a split are an error, with an opt-out (A2-14)", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 50, seed = 4)
  rowwise <- rsample::vfold_cv(long, v = 3)

  err <- expect_error(
    cv_joint_model(long, f_long, subject_id = "subject_id", resamples = rowwise, penalty = 0.05),
    "both the analysis and the assessment set"
  )
  expect_match(conditionMessage(err), "check_subject_overlap = FALSE", fixed = TRUE)

  # the opt-out allows deliberate overlap
  res <- cv_joint_model(
    long, f_long, subject_id = "subject_id", resamples = rowwise,
    check_subject_overlap = FALSE, penalty = 0.05
  )
  expect_s3_class(res, "cv_joint_model")
  expect_error(
    cv_joint_model(long, f_long, subject_id = "subject_id", resamples = rowwise, check_subject_overlap = NA),
    "check_subject_overlap"
  )

  # folds grouped by subject pass, and so do bootstraps (their out-of-bag
  # assessment sets never overlap the analysis set)
  grouped <- rsample::group_vfold_cv(long, group = subject_id, v = 3)
  res <- cv_joint_model(long, f_long, subject_id = "subject_id", resamples = grouped, penalty = 0.05)
  expect_true(all(is.finite(res$ibs)))
  boots <- rsample::group_bootstraps(long, group = subject_id, times = 3)
  res <- cv_joint_model(long, f_long, subject_id = "subject_id", resamples = boots, penalty = 0.05)
  expect_equal(nrow(res), 9L)
  expect_true(all(is.finite(res$ibs)))

  # right-censored data, no subject_id: a row is a subject, and a split that
  # scores rows it was fit on is the same leak
  df <- long[!duplicated(long$subject_id, fromLast = TRUE), ]
  df$time <- df$tstop
  f <- survival::Surv(time, status) ~ x1 + x2 + x3
  expect_no_error(cv_joint_model(df, f, resamples = rsample::vfold_cv(df, v = 3), penalty = 0.05))
  expect_error(
    cv_joint_model(df, f, resamples = rsample::bootstraps(df, times = 2, apparent = TRUE), penalty = 0.05),
    "both the analysis and the assessment set"
  )
})

test_that("nested_cv_joint_model checks its outer splits and inner resamples, with an opt-out (A2-14)", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 50, seed = 5)

  outer_leaks <- rsample::nested_cv(
    long,
    outside = rsample::vfold_cv(v = 3),
    inside = rsample::group_vfold_cv(group = subject_id, v = 2)
  )
  expect_error(
    nested_cv_joint_model(outer_leaks, f_long, subject_id = "subject_id", penalty = 0.05),
    "outer splits.*both the analysis and the assessment set"
  )

  inner_leaks <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::vfold_cv(v = 2)
  )
  expect_error(
    nested_cv_joint_model(inner_leaks, f_long, subject_id = "subject_id", penalty = 0.05),
    "inner resamples.*both the analysis and the assessment set"
  )
  res <- nested_cv_joint_model(
    inner_leaks, f_long, subject_id = "subject_id", penalty = 0.05, check_subject_overlap = FALSE
  )
  expect_s3_class(res, "nested_cv_joint_model")

  grouped <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::group_bootstraps(group = subject_id, times = 2)
  )
  res <- nested_cv_joint_model(grouped, f_long, subject_id = "subject_id", penalty = 0.05)
  expect_true(all(is.finite(res$ibs)))
})

test_that("score_surv_matrix_ibs is the yardstick integrated Brier score (A2-05)", {
  skip_if_not_installed("yardstick")
  skip_if_not_installed("survival")
  set.seed(7)
  n <- 80
  train <- survival::Surv(stats::rexp(100, 0.1) + 0.1, stats::rbinom(100, 1, 0.6))
  test <- survival::Surv(round(stats::rexp(n, 0.1) + 0.1, 2), stats::rbinom(n, 1, 0.6))
  et <- c(2, 4, 6, 8, 10)
  surv <- t(apply(matrix(stats::runif(n * 5, 0.1, 0.9), n, 5), 1, sort, decreasing = TRUE))
  cens <- censoring_km(train)

  scored <- tibble::tibble(
    .truth = test,
    .pred = lapply(seq_len(n), function(i) tibble::tibble(.eval_time = et, .pred_survival = surv[i, ]))
  )
  scored <- add_graf_weights(scored, censoring = cens)
  ref <- yardstick::brier_survival_integrated(scored, truth = .truth, .pred)$.estimate

  expect_equal(score_surv_matrix_ibs(surv, et, test, cens), ref)
  # one evaluation time: the Brier score at that time (yardstick needs two)
  one <- yardstick::brier_survival(scored, truth = .truth, .pred)
  expect_equal(score_surv_matrix_ibs(surv[, 3, drop = FALSE], et[3], test, cens), one$.estimate[3])
})

test_that("the coxnet score of cv_joint_model is the score cv_coxnet gives the same fit and folds (A2-02)", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 70, seed = 6)
  folds <- rsample::group_vfold_cv(long, group = subject_id, v = 3)
  et <- c(3, 6, 9, 12)
  path <- c(0.2, 0.1)
  metrics <- yardstick::metric_set(yardstick::brier_survival_integrated, yardstick::concordance_survival)

  from_cv_coxnet <- function(cov) {
    cv <- cv_coxnet(
      f_long, data = long, subject_id = "subject_id", penalty = path, resamples = folds,
      eval_time = et, covariates = cov, metrics = metrics
    )
    fm <- cv$fold_metrics[cv$fold_metrics$penalty == 0.1, ]
    list(
      ibs = stats::setNames(fm$.estimate[fm$.metric == "brier_survival_integrated"], fm$id[fm$.metric == "brier_survival_integrated"]),
      concordance = stats::setNames(fm$.estimate[fm$.metric == "concordance_survival"], fm$id[fm$.metric == "concordance_survival"])
    )
  }
  from_joint <- function(cov) {
    # the coxnet component is fit along the same two-penalty path, so the
    # coefficients at 0.1 are the ones cv_coxnet() scores
    jm <- cv_joint_model(
      long, f_long, resamples = folds, subject_id = "subject_id", calibration = FALSE,
      penalty = 0.1, path = path, eval_time = et, covariates = cov
    )
    jm <- jm[jm$model == "coxnet", ]
    list(ibs = stats::setNames(jm$ibs, jm$fold), concordance = stats::setNames(jm$concordance, jm$fold))
  }

  scores <- list()
  for (cov in c("path", "baseline")) {
    ref <- from_cv_coxnet(cov)
    got <- from_joint(cov)
    expect_equal(got$ibs[names(ref$ibs)], ref$ibs, tolerance = 1e-8)
    expect_equal(got$concordance[names(ref$concordance)], ref$concordance, tolerance = 1e-8)
    scores[[cov]] <- got$ibs
  }
  # the covariates of these subjects change over time, so the two rules differ
  expect_gt(max(abs(scores$path - scores$baseline[names(scores$path)])), 1e-4)

  # one row per subject (right-censored data): a row is a subject, as in cv_coxnet()
  df <- long[!duplicated(long$subject_id, fromLast = TRUE), ]
  df$time <- df$tstop
  f <- survival::Surv(time, status) ~ x1 + x2 + x3
  rows <- rsample::vfold_cv(df, v = 3)
  cv <- cv_coxnet(
    f, data = df, penalty = path, resamples = rows, eval_time = et,
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated)
  )
  ref <- cv$fold_metrics[cv$fold_metrics$penalty == 0.1, ]
  jm <- cv_joint_model(
    df, f, resamples = rows, calibration = FALSE, penalty = 0.1, path = path, eval_time = et
  )
  jm <- jm[jm$model == "coxnet", ]
  expect_equal(jm$ibs[match(ref$id, jm$fold)], ref$.estimate, tolerance = 1e-8)
})

test_that("covariates must be path or baseline, and the status and time models use the baseline row", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 40, seed = 8)
  expect_error(
    cv_joint_model(long, f_long, subject_id = "subject_id", covariates = "last", penalty = 0.05),
    "should be one of"
  )
  expect_error(
    nested_cv_joint_model(
      rsample::nested_cv(long, rsample::group_vfold_cv(group = subject_id, v = 2), rsample::group_vfold_cv(group = subject_id, v = 2)),
      f_long, subject_id = "subject_id", covariates = "last", penalty = 0.05
    ),
    "should be one of"
  )

  # first_interval_rows(): the earliest interval of every subject, in subject order
  subject <- c("b", "a", "b", "a", "a")
  start <- c(5, 3, 0, 0, 8)
  stop <- c(9, 8, 5, 3, 11)
  expect_identical(first_interval_rows(subject, start, stop), c(4L, 3L))
})

test_that("the status and time models are scored at each subject's baseline row (A2-02)", {
  skip_if_no_coxnet_deps()
  # Two intervals per subject; x1 is about 0 in the first and drives the event in the last.
  set.seed(21)
  long <- do.call(rbind, lapply(1:80, function(i) {
    z <- stats::rnorm(1)
    data.frame(
      subject_id = i, tstart = c(0, 4), tstop = c(4, 4 + stats::rexp(1, 0.2) + 0.5),
      status = c(0L, stats::rbinom(1, 1, stats::plogis(2 * z))),
      x1 = c(stats::rnorm(1, sd = 0.1), z), x2 = stats::rnorm(2), x3 = stats::rnorm(2)
    )
  }))
  analysis <- long[long$subject_id <= 55, ]
  assessment <- long[long$subject_id > 55, ]
  expect_true(any(duplicated(assessment$subject_id)))

  set.seed(1)
  got <- joint_model_fold_scores(
    analysis, assessment, f_long, "subject_id", "glmnet", FALSE, penalty = 0.05
  )

  # The same score from predict() on the first interval of each subject
  # (sim_counting() lists a subject's intervals in time order).
  set.seed(1)
  fit <- joint_model(analysis, f_long, subject_id = "subject_id", calibration = FALSE, penalty = 0.05)
  first <- assessment[!duplicated(assessment$subject_id), ]
  last <- assessment[!duplicated(assessment$subject_id, fromLast = TRUE), ]
  truth <- surv_subject_truth(
    survival::Surv(assessment$tstart, assessment$tstop, assessment$status), assessment$subject_id
  )
  cens <- censoring_km(surv_subject_truth(fit$components$surv_obj, analysis$subject_id)$.truth)
  et <- fit$eval_time
  proxy_ibs_at <- function(rows) {
    p <- stats::predict(fit, rows, eval_time = et)
    n <- nrow(rows)
    c(
      score_surv_matrix_ibs(matrix(1 - p$.pred_status_calibrated, n, length(et)), et, truth$.truth, cens),
      score_surv_matrix_ibs(
        matrix(as.numeric(p$.pred_time > rep(et, each = n)), n, length(et)), et, truth$.truth, cens
      )
    )
  }
  expect_equal(got$ibs$ibs[2:3], proxy_ibs_at(first))
  # the last interval would have given other numbers: the subjects' covariates change over time
  expect_gt(max(abs(proxy_ibs_at(first) - proxy_ibs_at(last))), 1e-4)
})

test_that("an assessment subject with an unseen factor level is left out of the scores of all three models (A2-06)", {
  skip_if_no_coxnet_deps()
  set.seed(10)
  n <- 100
  df <- data.frame(
    time = round(stats::rexp(n, 0.1) + 0.2, 2),
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    site = sample(c("a", "b", "c"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$site[7] <- "rare"
  assessment_rows <- as.integer(c(7, which(df$site != "rare")[1:30]))
  split <- rsample::make_splits(
    list(analysis = setdiff(seq_len(n), assessment_rows), assessment = assessment_rows), data = df
  )
  folds <- rsample::manual_rset(list(split), "Fold1")

  out <- collect_warnings(
    cv_joint_model(df, survival::Surv(time, status) ~ x1 + x2 + site, resamples = folds, penalty = 0.05)
  )
  # one warning that counts the subject left out, however many hardhat adds
  expect_equal(sum(grepl("1 of 31 subject\\(s\\)", out$warnings)), 1L)
  res <- out$value
  expect_setequal(res$model, c("coxnet", "status_calibrated", "time_regression"))
  # it used to be NaN for the coxnet model while the other two stayed finite
  expect_true(all(is.finite(res$ibs)))
  expect_true(all(is.finite(res$concordance)))

  # a split with no assessment subject left is skipped; with none at all, an error
  only_rare <- rsample::make_splits(list(analysis = setdiff(seq_len(n), 7L), assessment = 7L), data = df)
  expect_error(
    suppressWarnings(cv_joint_model(
      df, survival::Surv(time, status) ~ x1 + x2 + site,
      resamples = rsample::manual_rset(list(only_rare), "Fold1"), penalty = 0.05
    )),
    "No resample could be scored"
  )
})

test_that("joint_model stores the raw predictor columns beside the one-hot ones (A2-03)", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  set.seed(11)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1,
    status = stats::rbinom(n, 1, 0.6),
    x1 = stats::rnorm(n),
    grp = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + grp, penalty = 0.05)
  expect_named(fit$components$raw_predictors, c("x1", "grp"))
  expect_s3_class(fit$components$raw_predictors$grp, "factor")
  expect_equal(nrow(fit$components$raw_predictors), n)
  expect_setequal(fit$components$pred_names, c("x1", "grpa", "grpb", "grpc"))
})

test_that("joint_model_fold_scores' grouped folds keep a subject's rows together (A2-04)", {
  subject <- rep(1:25, times = c(rep(1:3, length.out = 25)))
  set.seed(2)
  fold <- joint_foldid(subject, 10L)
  expect_length(fold, length(subject))
  # all rows of a subject share a fold; there are 10 folds, none empty
  expect_true(all(tapply(fold, subject, function(f) length(unique(f))) == 1L))
  expect_setequal(fold, 1:10)
  # no more folds than subjects
  expect_equal(max(joint_foldid(c(1, 1, 2, 2, 3), 10L)), 3L)
})

test_that("the glmnet status and time models cross-validate with folds grouped by subject (A2-04)", {
  skip_if_no_coxnet_deps()
  long <- sim_counting(n = 50, seed = 12)
  seen <- list()
  real_cv_glmnet <- glmnet::cv.glmnet
  local_mocked_bindings(
    cv.glmnet = function(...) {
      args <- list(...)
      seen[[length(seen) + 1L]] <<- args$foldid
      do.call(real_cv_glmnet, args)
    },
    .package = "glmnet"
  )
  fit <- joint_model(long, f_long, subject_id = "subject_id", penalty = 0.05)
  expect_length(seen, 2L)  # status and time
  for (foldid in seen) {
    expect_false(is.null(foldid))
    expect_true(all(tapply(foldid, long$subject_id, function(f) length(unique(f))) == 1L))
  }
  expect_identical(seen[[1]], seen[[2]])
  expect_false(is.null(fit$calibration_model))
})

test_that("the status calibrator is fitted on out-of-fold predictions, not on in-sample ones (A2-04)", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("probably")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")

  # Pure noise: the outcome does not depend on the predictors. A bagged tree
  # classifies its own training rows almost perfectly, so a calibrator fitted
  # on those probabilities learns a steep slope and, on new data, pushes the
  # (uninformative) probabilities towards 0 and 1.
  noise <- function(n, seed) {
    set.seed(seed)
    data.frame(
      time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.5),
      x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n)
    )
  }
  train <- noise(200, 1)
  test <- noise(1000, 2)
  fit <- joint_model(train, survival::Surv(time, status) ~ ., engine = "baguette", penalty = 0.05)
  expect_false(is.null(fit$calibration_model))
  p <- predict(fit, test)
  expect_lte(stats::sd(p$.pred_status_calibrated), stats::sd(p$.pred_status))
  # and the calibrated probabilities stay close to the base rate
  expect_lt(abs(mean(p$.pred_status_calibrated) - mean(train$status)), 0.1)
})

test_that("the calibration is skipped with a warning when honest predictions cannot be produced (A2-04)", {
  skip_if_not_installed("probably")
  status <- factor(rep(c("event_free", "event"), 20), levels = c("event_free", "event"))

  # nothing out of fold, or too little of it: skipped, never replaced by in-sample predictions
  expect_warning(expect_null(fit_status_calibrator(NULL, status)), "uncalibrated")
  expect_warning(expect_null(fit_status_calibrator(rep(NA_real_, 40), status)), "uncalibrated")
  expect_warning(expect_null(fit_status_calibrator(rep(0.4, 5), status[1:5])), "uncalibrated")
  expect_warning(expect_null(fit_status_calibrator(rep(0.4, 20), status[rep(2, 20)])), "uncalibrated")

  prob <- stats::plogis(stats::rnorm(40))
  expect_no_warning(cal <- fit_status_calibrator(prob, status))
  expect_false(is.null(cal))
  # a missing out-of-fold probability costs only its own row
  prob[3] <- NA
  expect_no_warning(fit_status_calibrator(prob, status))
})

test_that("a joint model without out-of-fold predictions is returned uncalibrated, with a warning (A2-04)", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("probably")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  set.seed(13)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.5),
    x1 = stats::rnorm(n), x2 = stats::rnorm(n)
  )
  local_mocked_bindings(bagged_out_of_fold_status = function(...) NULL)
  expect_warning(
    fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2, engine = "baguette", penalty = 0.05),
    "uncalibrated"
  )
  expect_null(fit$calibration_model)
  p <- predict(fit, df[1:5, ])
  expect_equal(p$.pred_status_calibrated, p$.pred_status)
})

test_that("engine = \"stacks\" warns that its main predictions equal baguette's, adds the meta-learner columns, and a failing meta-learner is not silent (A2-08)", {
  skip_if_not_installed("baguette")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
  set.seed(14)
  n <- 60
  df <- data.frame(
    time = stats::rexp(n, 0.1) + 0.1, status = stats::rbinom(n, 1, 0.5),
    x1 = stats::rnorm(n), x2 = stats::rnorm(n)
  )
  f <- survival::Surv(time, status) ~ x1 + x2
  expect_warning(
    fit <- joint_model(df, f, engine = "stacks", calibration = FALSE, penalty = 0.05),
    "same bagged trees as `engine = \"baguette\"`"
  )
  expect_s3_class(fit$stack_model, "cv.glmnet")
  base_cols <- c(".pred_survival", ".pred_status", ".pred_status_calibrated", ".pred_time", ".pred_linear_pred", ".pred_risk_score")
  # predict() adds the meta-learner's linear predictor and risk score to the baguette columns
  pred <- predict(fit, df[1:3, ])
  expect_setequal(names(pred), c(base_cols, ".pred_stack_linear_pred", ".pred_stack_risk_score"))
  expect_true(all(is.finite(pred$.pred_stack_linear_pred)))
  expect_equal(pred$.pred_stack_risk_score, exp(-pred$.pred_stack_linear_pred))
  expect_named(predict(fit, df[1:3, ], type = "stack_linear_pred"), ".pred_stack_linear_pred")
  expect_no_warning(fit_bag <- joint_model(df, f, engine = "baguette", calibration = FALSE, penalty = 0.05))
  expect_setequal(names(predict(fit_bag, df[1:3, ])), base_cols)

  # the meta-learner used to fail silently into NULL (or a fixed-lambda fallback)
  real_cv_glmnet <- glmnet::cv.glmnet
  local_mocked_bindings(
    cv.glmnet = function(x, y, family = "gaussian", ...) {
      if (family == "cox") stop("meta fit failed") else real_cv_glmnet(x, y, family = family, ...)
    },
    .package = "glmnet"
  )
  out <- collect_warnings(joint_model(df, f, engine = "stacks", calibration = FALSE, penalty = 0.05))
  expect_true(any(grepl("meta-learner.*could not be fitted \\(meta fit failed\\)", out$warnings)))
  expect_null(out$value$stack_model)
  # without a meta-learner predict() returns the baguette columns only
  expect_setequal(names(predict(out$value, df[1:3, ])), base_cols)
})
