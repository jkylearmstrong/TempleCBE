# Feature-integration fixes from the round-2 fresh review
# (TempleCBE-gaps-round2-2026-11-12.md, section E, items 35-42).
#
# 35: validation adapters accept fine_gray_model / cox_time_varying, and
#     extract_horizon_risk() reads the .pred_cif shape.
# 36: joint_readmission_death()'s $death_fit works with basehaz()/survfit().
# 37: calibration_from_model() documents that no summary metrics exist for
#     Surv outcomes (docs only; the recipe in the help is exercised here).
# 38: read_sas_data() example uses an 8-character XPT stem (rossi.xpt).
# 39: the generated Python loader omits load_data() when no data file exists.
# 40: README narrows the "two-way reticulate" claim to one-way export.
# 41: the competing-risks and reticulate vignettes demonstrate fine_gray() /
#     predict_fine_gray() and export_to_python().
# 42: tune_impute_dataset() documents the forest engines' whole-data sweep
#     and that the workflows engine does not chain imputations (docs only).

# Fixtures (local copies of the wave-2 fixtures, so this file is standalone) --

r2_competing_data <- function(seed = 1, n = 150) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.1),
    type = sample(0:2, n, TRUE, prob = c(0.3, 0.4, 0.3)),
    age = stats::rnorm(n, 60, 8), arm = rep(c("A", "B"), length.out = n)
  )
}

r2_surv_data <- function(seed = 1, n = 200) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
}

r2_sim_readmission_death <- function(n = 120, seed = 1) {
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

r2_root <- function() file.path(testthat::test_path(), "..", "..")

# Item 35 ---------------------------------------------------------------------

test_that("extract_horizon_risk() reads Fine-Gray .pred_cif curves as the risk itself (35)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  fit <- fine_gray(d, "time", "type", ~ age + arm)
  pr <- predict(fit, d[1:3, ], times = c(5, 10))
  risk <- extract_horizon_risk(pr, 10)
  expect_true(all(risk >= 0 & risk <= 1))
  # Cross-check against cmprsk::predict.crr: the CIF at the latest event
  # time at or before 10, per subject.
  mm <- stats::model.matrix(fit$terms, data = d[1:3, ])[, -1, drop = FALSE]
  pc <- cmprsk::predict.crr(fit$fit, cov1 = as.matrix(mm))
  at10 <- pc[pc[, 1] <= 10, , drop = FALSE]
  expect_equal(unname(risk), unname(at10[nrow(at10), -1]), tolerance = 1e-6)
  # The CIF is not one minus a survival probability: it rises with time.
  expect_true(all(extract_horizon_risk(pr, 10) >= extract_horizon_risk(pr, 5)))
  # A curve with neither value column, and mixed value columns, both error.
  bad <- pr
  bad$.pred[[1]]$.pred_cif <- NULL
  expect_error(extract_horizon_risk(bad, 10), "pred_cif")
  mixed <- pr
  mixed$.pred[[1]] <- tibble::tibble(.eval_time = c(5, 10), .pred_survival = c(0.9, 0.8))
  expect_error(extract_horizon_risk(mixed, 10), "same value column")
})

test_that("predict_event_risk() works for fine_gray_model and cox_time_varying (35)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  fg <- fine_gray(d, "time", "type", ~ age + arm)
  risk <- predict_event_risk(fg, d[1:3, ], horizon = 10)
  expect_equal(risk, extract_horizon_risk(predict(fg, d[1:3, ], times = 10), 10))
  # failcode = 2 is the CIF of the other cause, not of cause 1.
  fg2 <- fine_gray(d, "time", "type", ~ age + arm, failcode = 2)
  expect_false(isTRUE(all.equal(
    predict_event_risk(fg2, d[1:3, ], horizon = 10), risk
  )))
  # Character event codes work as well.
  dfac <- d
  dfac$type <- factor(d$type, labels = c("cens", "cause1", "cause2"))
  fgc <- fine_gray(dfac, "time", "type", ~ age + arm, failcode = "cause1", cencode = "cens")
  expect_equal(
    predict_event_risk(fgc, dfac[1:3, ], horizon = 10), risk, tolerance = 1e-6
  )

  d2 <- r2_surv_data()
  tv <- cox_time_varying(d2, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  tv_risk <- predict_event_risk(tv, d2[1:3, ], horizon = 10)
  expect_equal(
    tv_risk,
    extract_horizon_risk(predict(tv, d2[1:3, ], type = "survival", eval_time = 10), 10)
  )
  expect_true(all(tv_risk >= 0 & tv_risk <= 1))
})

test_that("calibration_from_model() / decision_curve_from_model() accept fine_gray and cox_time_varying fits (35)", {
  skip_if_not_installed("cmprsk")
  d <- r2_competing_data()
  fg <- fine_gray(d, "time", "type", ~ age + arm)
  ct <- calibration_from_model(fg, d, "time", "type", horizon = 10, bins = 5)
  expect_equal(nrow(ct), 5L)
  expect_true(all(ct$mean_pred >= 0 & ct$mean_pred <= 1))
  expect_true(all(ct$observed >= 0 & ct$observed <= 1, na.rm = TRUE))
  # The calibration is for the cause of interest: with failcode = 2 the
  # predicted risks (and the dichotomised truth) change.
  fg2 <- fine_gray(d, "time", "type", ~ age + arm, failcode = 2)
  ct2 <- calibration_from_model(fg2, d, "time", "type", horizon = 10, bins = 5)
  expect_false(isTRUE(all.equal(ct$mean_pred, ct2$mean_pred)))
  dc <- decision_curve_from_model(fg, d, "time", "type", horizon = 10)
  expect_true(all(dc$net_benefit <= 1, na.rm = TRUE))
  expect_true(all(dc$prop_above >= 0 & dc$prop_above <= 1))

  d2 <- r2_surv_data()
  tv <- cox_time_varying(d2, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  ct_tv <- calibration_from_model(tv, d2, "time", "status", horizon = 10, bins = 5)
  expect_equal(nrow(ct_tv), 5L)
  dc_tv <- decision_curve_from_model(tv, d2, "time", "status", horizon = 10)
  expect_true(nrow(dc_tv) > 0L)
})

# Item 36 ---------------------------------------------------------------------

test_that("joint_readmission_death() $death_fit works with basehaz() and survfit() (36)", {
  skip_if_not_installed("survival")
  long <- r2_sim_readmission_death()
  fit <- joint_readmission_death(long, ~ age, ~ age)
  expect_false(is.null(fit$death_fit$model))
  bh <- survival::basehaz(fit$death_fit)
  expect_true(all(c("hazard", "time") %in% names(bh)))
  expect_true(all(is.finite(bh$hazard)) && all(diff(bh$hazard) >= 0))
  nd <- data.frame(age = c(-1, 0, 1), n_prior_readmissions = 0)
  sf <- survival::survfit(fit$death_fit, newdata = nd)
  expect_s3_class(sf, "survfit")
  expect_true(all(sf$surv >= 0 & sf$surv <= 1))
  # The package's own predict() still agrees in shape after the change.
  sv <- predict(fit, data.frame(age = 0), type = "death_survival", eval_time = c(30, 120))
  expect_equal(nrow(sv), 1L)
})

# Item 37 ---------------------------------------------------------------------

test_that("calibration_from_model() metrics are NA for Surv outcomes, and the documented slope recipe works (37)", {
  skip_if_not_installed("glmnet")
  # Data with real signal (unlike the null fixtures above): the hazard
  # depends on age and sex, so the predicted risks vary and the slope in
  # the documented recipe is estimable.
  set.seed(1)
  n <- 200
  age <- stats::rnorm(n, 60, 10)
  sex <- stats::rbinom(n, 1, 0.5)
  ev_time <- stats::rexp(n, 0.02 * exp(0.04 * (age - 60) + 0.5 * sex))
  cens_time <- stats::rexp(n, 0.01)
  d <- data.frame(
    time = pmin(ev_time, cens_time), status = as.integer(ev_time <= cens_time),
    age = age, sex = sex
  )
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.01)
  ct <- calibration_from_model(fit, d, "time", "status", horizon = 10, bins = 5)
  mets <- attr(ct, "metrics")
  expect_true(all(is.na(mets$intercept)) && all(is.na(mets$slope)) && all(is.na(mets$brier)))
  # The recipe documented in ?calibration_from_model: dichotomise at the
  # horizon (dropping subjects censored before it) and regress on the logit
  # of the predicted risk.
  risk <- predict_event_risk(fit, d, horizon = 10)
  expect_gt(stats::sd(risk), 0)
  known <- d$time > 10 | d$status == 1
  y_h <- as.integer(d$time <= 10 & d$status == 1)[known]
  slope <- unname(stats::coef(stats::glm(
    y_h ~ stats::qlogis(pmin(pmax(risk[known], 1e-8), 1 - 1e-8)),
    family = stats::binomial()
  ))[2L])
  expect_true(is.finite(slope))
})

# Item 38 ---------------------------------------------------------------------

test_that("read_sas_data() example uses an 8-character XPT stem and round-trips (38)", {
  skip_if_not_installed("haven")
  p <- file.path(tempdir(), "rossi.xpt")
  write_sas_data(rossi_data(), p)
  back <- read_sas_data(p)
  expect_equal(nrow(back), 432L)
  expect_equal(names(back), names(rossi_data()))
  expect_equal(back$week, rossi_data()$week)
  # The documented example itself must name rossi.xpt, not rossi_xpt.xpt.
  rd <- readLines(file.path(r2_root(), "man", "read_sas_data.Rd"), warn = FALSE)
  expect_true(any(grepl("rossi.xpt", rd, fixed = TRUE)))
  expect_false(any(grepl("rossi_xpt", rd, fixed = TRUE)))
  r_src <- readLines(file.path(r2_root(), "R", "sas_io_data.R"), warn = FALSE)
  expect_false(any(grepl("rossi_xpt", r_src, fixed = TRUE)))
})

# Item 39 ---------------------------------------------------------------------

test_that("export_to_python() loader omits load_data() for model exports but keeps it for data exports (39)", {
  skip_if_not_installed("glmnet")
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog", "wt.loss")])
  fit <- coxnet(survival::Surv(time, status) ~ ., data = lung, penalty = 0.05, mixture = 0.5)
  dir <- file.path(tempdir(), "pyexp39_model")
  out <- export_to_python(fit, path = dir, name = "cox", new_data = lung[1:3, ],
                          eval_time = c(180, 365), overwrite = TRUE)
  expect_false(file.exists(file.path(dir, "cox_data.csv")))
  loader <- readLines(file.path(dir, "cox_load.py"))
  expect_false(any(grepl("def load_data", loader, fixed = TRUE)))
  expect_true(any(grepl("def load_coefficients", loader, fixed = TRUE)))
  expect_true(any(grepl("def load_survival", loader, fixed = TRUE)))
  expect_true(any(grepl("def load_manifest", loader, fixed = TRUE)))
  # A named-list export also writes no data file, so its loader omits it too.
  dir_lst <- file.path(tempdir(), "pyexp39_list")
  export_to_python(list(a = head(mtcars, 5)), path = dir_lst, name = "lst", overwrite = TRUE)
  loader_lst <- readLines(file.path(dir_lst, "lst_load.py"))
  expect_false(any(grepl("def load_data", loader_lst, fixed = TRUE)))
  # A data-frame export still defines load_data().
  dir_df <- file.path(tempdir(), "pyexp39_data")
  export_to_python(head(mtcars, 10), path = dir_df, name = "demo", overwrite = TRUE)
  loader_df <- readLines(file.path(dir_df, "demo_load.py"))
  expect_true(any(grepl("def load_data", loader_df, fixed = TRUE)))
  # In real Python the model loader has no load_data attribute at all, and
  # the data loader's load_data() reads the file back.
  skip_if(Sys.which("python3") == "", "python3 not available")
  py <- tempfile(fileext = ".py")
  writeLines(c(
    "import importlib.util, sys",
    sprintf("for stem, d, want in [('cox', %s, False), ('demo', %s, True)]:", deparse(dir), deparse(dir_df)),
    "    spec = importlib.util.spec_from_file_location('loader_' + stem, d + '/' + stem + '_load.py')",
    "    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)",
    "    assert hasattr(mod, 'load_data') == want, (stem, hasattr(mod, 'load_data'))",
    "    assert hasattr(mod, 'load_manifest')",
    "rows = mod.load_data() if want else None",
    "assert rows is None or len(rows) == 10, len(rows)",
    "print('python loader checks OK')"
  ), py)
  expect_equal(system2("python3", shQuote(py), stdout = TRUE, stderr = TRUE), "python loader checks OK")
})

# Item 40 ---------------------------------------------------------------------

test_that("README narrows the Python claim to one-way export (40)", {
  skip_if_not(file.exists(file.path(r2_root(), "README.md")), "README.md not in the built package")
  skip_if_not(file.exists(file.path(r2_root(), "README.qmd")), "README.qmd not in the built package")
  for (f in c("README.md", "README.qmd")) {
    text <- paste(readLines(file.path(r2_root(), f), warn = FALSE), collapse = "\n")
    expect_match(text, "One-way export to Python", fixed = TRUE)
    expect_false(grepl("two-way", text, fixed = TRUE))
  }
})

# Item 41 ---------------------------------------------------------------------

test_that("vignettes demonstrate fine_gray()/predict_fine_gray() and export_to_python() (41)", {
  skip_if_not(file.exists(file.path(r2_root(), "vignettes", "competing_risks_and_imputation.Rmd")),
              "vignettes are not in the built package")
  cr <- paste(readLines(file.path(r2_root(), "vignettes", "competing_risks_and_imputation.Rmd"),
                        warn = FALSE), collapse = "\n")
  expect_match(cr, "fine_gray(d", fixed = TRUE)
  expect_match(cr, "predict(fg, d[1:3, ]", fixed = TRUE)
  expect_match(cr, "predict_event_risk()", fixed = TRUE)
  rx <- paste(readLines(file.path(r2_root(), "vignettes", "reticulate_export.Rmd"),
                        warn = FALSE), collapse = "\n")
  expect_match(rx, "export_to_python(fit_py", fixed = TRUE)
  expect_match(rx, "sim_coxnet_survival.csv", fixed = TRUE)
  # The reticulate vignette's new chunk runs: a coxnet fit exports with a
  # survival matrix, as the vignette shows.
  skip_if_not_installed("glmnet")
  set.seed(42)
  n <- 120
  sim_cohort <- as.data.frame(matrix(stats::rnorm(n * 6), n, 6))
  names(sim_cohort) <- paste0("X", 1:6)
  sim_cohort$time <- stats::rexp(n, rate = exp(1.2 * sim_cohort$X1 - 0.8 * sim_cohort$X2))
  sim_cohort$status <- stats::rbinom(n, 1, 0.7)
  fit_py <- coxnet(survival::Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6,
                   data = sim_cohort, penalty = 0.1)
  py_dir <- file.path(tempdir(), "templecbe_py41")
  out_py <- export_to_python(fit_py, path = py_dir, name = "sim_coxnet",
                             new_data = sim_cohort[1:3, ], eval_time = c(5, 10),
                             overwrite = TRUE)
  expect_true(file.exists(file.path(py_dir, "sim_coxnet_survival.csv")))
  expect_equal(nrow(utils::read.csv(file.path(py_dir, "sim_coxnet_survival.csv"))), 3L)
})

# Item 42 ---------------------------------------------------------------------

test_that("tune_impute_dataset() documents the forest whole-data sweep and no chaining (42)", {
  rd <- paste(readLines(file.path(r2_root(), "man", "tune_impute_dataset.Rd"), warn = FALSE),
              collapse = "\n")
  expect_match(rd, "Forest engines impute the whole data set", fixed = TRUE)
  expect_match(rd, "incomplete column filled", fixed = TRUE)
  expect_match(rd, "not chained", fixed = TRUE)
  # Code-read check: the forest branch passes the whole data frame to the
  # sweep (not a targets-only subset), which is what the docs describe.
  src <- paste(readLines(file.path(r2_root(), "R", "tune_impute_workflows.R"), warn = FALSE),
               collapse = "\n")
  expect_match(src, "sweep <- sweep_fn(data, exclude = exclude", fixed = TRUE)
})
