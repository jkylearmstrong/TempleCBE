# Draft Part 2 data/SAS/Python features (fresh-review A2, A3, B2, B4, B5).
# B5: the SAS 9.4 LIFETEST run has happened — the listing and log are stored
# under inst/sas/list/ and inst/sas/logs/, and the Tier-1 test below parses
# the stored listing and compares it with the R quantities. The hand-computed
# values and the synthetic listing are still used for the R-side unit tests.
# The workflowsets/finetune/tune/parsnip and missForest/missRanger tests are
# skipped when those Suggests packages are absent (as on this machine); CI
# exercises them.

lifetest_benchmark_data <- function() {
  data.frame(
    time = c(5L, 6L, 6L, 8L, 10L, 12L, 3L, 4L, 7L, 9L, 9L, 11L),
    status = c(1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L),
    group = rep(c("A", "B"), each = 6)
  )
}

# --- A2: Python export -------------------------------------------------------

test_that("export_to_python() writes a data frame, manifest and working loader", {
  dir <- file.path(tempdir(), "pyexp_data")
  out <- export_to_python(head(mtcars, 10), path = dir, name = "demo")
  expect_s3_class(out, "python_export")
  expect_true(all(file.exists(out$files)))
  expect_true(file.exists(file.path(dir, "demo_data.csv")))
  expect_equal(nrow(utils::read.csv(file.path(dir, "demo_data.csv"))), 10)
  manifest <- jsonlite::fromJSON(file.path(dir, "demo_manifest.json"))
  expect_equal(manifest$package, "TempleCBE")
  expect_equal(manifest$object_class, "data.frame")
  expect_true("demo_data.csv" %in% manifest$files)
  loader <- readLines(file.path(dir, "demo_load.py"))
  expect_true(any(grepl("def load_data", loader, fixed = TRUE)))
  expect_true(any(grepl("def load_manifest", loader, fixed = TRUE)))
})

test_that("export_to_python() exports a fitted coxnet model with survival matrix and dictionary", {
  skip_if_not_installed("glmnet")
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog", "wt.loss")])
  fit <- coxnet(survival::Surv(time, status) ~ ., data = lung, penalty = 0.05, mixture = 0.5)
  dir <- file.path(tempdir(), "pyexp_cox")
  dict <- data.frame(term = c("age", "sex"), label = c("Age (years)", "Sex"))
  out <- export_to_python(
    fit, path = dir, name = "cox", new_data = lung[1:3, ], eval_time = c(180, 365), dictionary = dict
  )
  expect_true(file.exists(file.path(dir, "cox_coefficients.csv")))
  expect_true(file.exists(file.path(dir, "cox_model.json")))
  expect_true(file.exists(file.path(dir, "cox_dictionary.csv")))
  expect_true(file.exists(file.path(dir, "cox_dictionary.json")))
  surv <- utils::read.csv(file.path(dir, "cox_survival.csv"))
  expect_equal(nrow(surv), 3)
  expect_true(all(c("t_180", "t_365") %in% names(surv)))
  expect_true(all(surv$t_180 >= 0 & surv$t_180 <= 1))
  meta <- jsonlite::fromJSON(file.path(dir, "cox_model.json"))
  expect_equal(meta$kind, "coxnet_model")
  expect_equal(meta$mixture, 0.5)
})

test_that("export_to_python() validates its inputs and refuses to overwrite", {
  dir <- file.path(tempdir(), "pyexp_err")
  expect_error(export_to_python(mtcars, path = dir, name = "a/b"), "without a path separator")
  expect_error(export_to_python(list(1, 2), path = dir), "must be named")
  expect_error(export_to_python(mean, path = dir), "must be a data frame")
  expect_error(
    export_to_python(mtcars, path = dir, name = "x", eval_time = 100),
    "must be given together"
  )
  export_to_python(head(mtcars), path = dir, name = "x")
  expect_error(export_to_python(head(mtcars), path = dir, name = "x"), "already exists")
  expect_no_error(export_to_python(head(mtcars), path = dir, name = "x", overwrite = TRUE))
})

test_that("export_to_python(format = 'pickle') needs reticulate", {
  skip_if_not_installed("reticulate")
  # Only reached where reticulate exists; elsewhere the error below is the test.
  dir <- file.path(tempdir(), "pyexp_pkl")
  out <- export_to_python(head(mtcars), path = dir, name = "x", format = "pickle")
  expect_true(file.exists(file.path(dir, "x.pkl")))
})

test_that("export_to_python(format = 'pickle') errors clearly without reticulate", {
  skip_if(requireNamespace("reticulate", quietly = TRUE), "reticulate is installed")
  expect_error(
    export_to_python(head(mtcars), path = file.path(tempdir(), "pyexp_nopkl"), format = "pickle"),
    "reticulate"
  )
})

# --- A3: SAS I/O wrappers and parity boundary --------------------------------

test_that("write_sas_data()/read_sas_data() round-trip .xpt and .sas7bdat", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:5, age = c(20, 21, 19, 22, 20), sex = c(0, 1, 0, 1, 0))
  p_xpt <- file.path(tempdir(), "rt.xpt")
  write_sas_data(d, p_xpt)
  back <- read_sas_data(p_xpt)
  expect_equal(nrow(back), 5)
  expect_equal(as.numeric(back$age), d$age)
  p_sas <- file.path(tempdir(), "rt.sas7bdat")
  expect_warning(write_sas_data(d, p_sas), "deprecated")
  back2 <- read_sas_data(p_sas)
  expect_equal(nrow(back2), 5)
  expect_equal(as.numeric(back2$id), 1:5)
})

test_that("write_sas_data()/read_sas_data() validate names, extensions and paths", {
  skip_if_not_installed("haven")
  d <- data.frame(a = 1:3)
  expect_error(write_sas_data(d, file.path(tempdir(), "x.txt")), "Cannot infer the format")
  expect_error(
    write_sas_data(data.frame(this_name_is_far_too_long_for_sas = 1), file.path(tempdir(), "x.sas7bdat")),
    "at most 32 characters"
  )
  expect_error(
    write_sas_data(data.frame(longname1 = 1), file.path(tempdir(), "x.xpt")),
    "at most 8 characters"
  )
  expect_error(
    write_sas_data(data.frame(a = 1), file.path(tempdir(), "roundtrip.xpt")),
    "longer than 8 characters"
  )
  expect_error(
    write_sas_data(data.frame(`bad name` = 1, check.names = FALSE), file.path(tempdir(), "x.sas7bdat")),
    "Not valid SAS variable names"
  )
  expect_error(write_sas_data(list(a = 1), file.path(tempdir(), "x.sas7bdat")), "must be a data frame")
  expect_error(read_sas_data(file.path(tempdir(), "does_not_exist.sas7bdat")), "existing file")
  txt <- file.path(tempdir(), "x.txt")
  writeLines("a,b", txt)
  expect_error(read_sas_data(txt, format = "auto"), "Cannot infer the format")
})

test_that("sas_parity_boundary() documents the boundary, including LIFETEST and follow-ups", {
  b <- sas_parity_boundary()
  expect_true(all(c("proc", "r_counterpart", "status", "evidence", "notes") %in% names(b)))
  expect_true(all(b$status %in% c("parity", "partial", "not_started")))
  expect_equal(b$status[b$proc == "PROC PHREG"], "parity")
  expect_equal(b$status[b$proc == "PROC COMPARE"], "parity")
  expect_equal(b$status[b$proc == "PROC LIFETEST"], "parity")
  expect_equal(b$status[b$proc == "PROC LOGISTIC"], "not_started")
  expect_equal(b$status[b$proc == "PROC MIXED / GLIMMIX"], "not_started")
  expect_equal(b$status[b$proc == "PROC FREQ"], "not_started")
  expect_equal(b$status[b$proc == "PROC MEANS"], "not_started")
})

# --- B5: PROC LIFETEST parity (SAS 9.4 run stored; Tier-1 test below) --------

test_that("benchmark_lifetest_km.sas holds the benchmark data typed in R", {
  sas <- sas_datalines(
    sas_macro_path("benchmark_lifetest_km.sas"),
    c("group", "time", "status"),
    csv = TRUE
  )
  r <- lifetest_benchmark_data()
  expect_equal(as.data.frame(sas), r[, c("group", "time", "status")], ignore_attr = TRUE)
})

test_that("cbe_lifetest.sas defines a PROC LIFETEST macro", {
  path <- sas_macro_path("cbe_lifetest.sas")
  expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("%macro cbe_lifetest", txt, fixed = TRUE))
  expect_true(grepl("proc lifetest", txt, fixed = TRUE))
  expect_true(grepl("HomTests", txt, fixed = TRUE))
})

test_that("km_lifetest_quantities() matches hand-computed Kaplan-Meier values", {
  q <- km_lifetest_quantities(lifetest_benchmark_data(), "time", "status", group = "group")
  surv <- function(term, time) q$value[q$quantity == "km_surv" & q$term == term & q$time == time]
  # Stratum A: 5/6, then x4/5, then x2/3, then x1/2.
  expect_equal(surv("A", 5), 5 / 6)
  expect_equal(surv("A", 6), 2 / 3)
  expect_equal(surv("A", 8), 4 / 9)
  expect_equal(surv("A", 10), 2 / 9)
  # Stratum B: 5/6, then x4/5, then x3/4, then x2/3.
  expect_equal(surv("B", 3), 5 / 6)
  expect_equal(surv("B", 4), 2 / 3)
  expect_equal(surv("B", 7), 1 / 2)
  expect_equal(surv("B", 9), 1 / 3)
  # SAS medians: first event time at which survival is at most 0.5, the midpoint of a flat step at exactly 0.5.
  med <- q$value[q$quantity == "median"]
  expect_equal(med[q$term[q$quantity == "median"] == "A"], 8)
  expect_equal(med[q$term[q$quantity == "median"] == "B"], 8)
  # Log-rank agrees with survdiff() on the same data.
  lr <- survival::survdiff(
    survival::Surv(time, status) ~ group, data = lifetest_benchmark_data()
  )
  expect_equal(q$value[q$quantity == "logrank_chisq"], unname(lr$chisq))
  expect_equal(q$value[q$quantity == "logrank_df"], 1)
  expect_equal(
    q$value[q$quantity == "logrank_p"],
    stats::pchisq(unname(lr$chisq), 1, lower.tail = FALSE)
  )
})

test_that("km_lifetest_quantities() validates its inputs", {
  d <- lifetest_benchmark_data()
  expect_error(km_lifetest_quantities(d, "time", "status2"), "must name one column")
  expect_error(km_lifetest_quantities(d, "group", "status"), "finite, non-negative")
  bad <- d
  bad$status[1] <- 2
  expect_error(km_lifetest_quantities(bad, "time", "status"), "must be 0/1")
})

test_that("parse_lifetest_lst() reads a SAS 9.4 listing and compare_km_lifetest() agrees", {
  lst <- c(
    "Stratum A",
    "Product-Limit Survival Estimates",
    "    5.0000    0.8333    0.1667    0.1521    1    5",
    "    6.0000    0.6667    0.3333    0.1925    1    4",
    "    8.0000    0.4444    0.5556    0.2028    1    2",
    "    10.0000    0.2222    0.7778    0.1700    1    1",
    "Quartiles",
    "    50% (Median)    8.0000",
    "Stratum B",
    "Product-Limit Survival Estimates",
    "    3.0000    0.8333    0.1667    0.1521    1    5",
    "    4.0000    0.6667    0.3333    0.1925    1    4",
    "    7.0000    0.5000    0.5000    0.2041    1    3",
    "    9.0000    0.3333    0.6667    0.1925    1    2",
    "Quartiles",
    "    50% (Median)    8.0000",
    "Test of Equality over Strata",
    "    Log-Rank    0.0198    1    0.8880"
  )
  sas <- parse_lifetest_lst(lst)
  expect_equal(nrow(sas[sas$quantity == "km_surv", ]), 8)
  expect_equal(sas$value[sas$quantity == "median" & sas$term == "B"], 8)
  r <- km_lifetest_quantities(lifetest_benchmark_data(), "time", "status", group = "group")
  cmp <- compare_km_lifetest(r, sas, atol = 1e-4, rtol = 1e-3)
  expect_true(cmp$matched)
  # A perturbed SAS value must fail the comparison, not pass silently.
  sas_bad <- sas
  sas_bad$value[sas_bad$quantity == "km_surv" & sas_bad$term == "A" & sas_bad$time == 5] <- 0.5
  expect_false(compare_km_lifetest(r, sas_bad, atol = 1e-4, rtol = 1e-3)$matched)
  expect_error(parse_lifetest_lst("nothing to parse here"), "could not be parsed")
})

# --- B2: whole-dataset imputation tuning -------------------------------------

test_that("tune_impute_dataset() validates targets without needing the tuning packages", {
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  d$hp[c(5, 15)] <- NA
  d$grp <- factor(rep(c("x", "y"), each = 16))
  d$grp[c(1, 2)] <- NA
  expect_error(tune_impute_dataset(d, targets = "grp"), "numeric columns only")
  expect_error(tune_impute_dataset(d, targets = "mpg", exclude = "mpg"), "must not overlap")
  expect_error(tune_impute_dataset(mtcars), "No incomplete numeric columns")
  # the engine packages are only checked when absent
  if (!requireNamespace("missForest", quietly = TRUE)) {
    expect_error(tune_impute_dataset(d, engine = "missforest", parallel = FALSE), "missForest")
  }
  if (!requireNamespace("missRanger", quietly = TRUE)) {
    expect_error(tune_impute_dataset(d, engine = "missranger", parallel = FALSE), "missRanger")
  }
})

test_that("tune_impute_dataset() tunes every incomplete variable (workflows engine)", {
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("finetune")
  skip_if_not_installed("tune")
  skip_if_not_installed("parsnip")
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  d$hp[c(5, 15)] <- NA
  res <- tune_impute_dataset(d, folds = 3, race = FALSE)
  expect_s3_class(res, "impute_tune_dataset")
  expect_equal(sort(res$targets), c("hp", "mpg"))
  expect_equal(nrow(res$summary), 2)
  expect_true(all(c("best_workflow_id", "rmse", "rse", "n_imputed") %in% names(res$summary)))
  expect_equal(unname(res$summary$n_imputed[res$summary$variable == "mpg"]), 3)
  expect_equal(unname(res$summary$n_imputed[res$summary$variable == "hp"]), 2)
  expect_false(anyNA(res$data$mpg))
  expect_false(anyNA(res$data$hp))
  expect_equal(nrow(res$data), nrow(d))
})

test_that("tune_impute_dataset() with the missforest engine sweeps mtry over the whole data", {
  skip_if_not_installed("missForest")
  d <- data.frame(
    a = c(1, 2, NA, 4, 5, 6, NA, 8),
    b = c(2, NA, 6, 8, 10, 12, 14, NA),
    c = c(5, 4, 3, NA, 1, 2, 3, 4)
  )
  res <- tune_impute_dataset(d, engine = "missforest", parallel = FALSE, seed = 42)
  expect_equal(sort(res$targets), c("a", "b", "c"))
  expect_false(anyNA(res$data))
  expect_true(all(c("best_mtry", "best_error") %in% names(res$summary)))
})

test_that("missforest/missranger sweeps name themselves in the empty-shard error (not `fn`)", {
  # A shard covering no mtry value must error with the function's own name;
  # before the fix this errored with "object 'fn' not found" instead.
  d <- data.frame(
    a = c(1, 2, NA, 4, 5, 6, 7, 8),
    b = c(2, NA, 6, 8, 10, 12, 14, 16),
    c = c(5, 4, 3, NA, 1, 2, 3, 4)
  )
  expect_error(
    missforest_sweep_mtry(d, mtry_values = 1:2, parallel = FALSE,
                          checkpoint_dir = file.path(tempdir(), "ckpt_f"), shard = c(3, 3)),
    "missforest_sweep_mtry.*evaluated none"
  )
  expect_error(
    missranger_sweep_mtry(d, mtry_values = 1, parallel = FALSE,
                          checkpoint_dir = file.path(tempdir(), "ckpt_r"), shard = c(2, 2)),
    "missranger_sweep_mtry.*evaluated none"
  )
})

# --- B4: imputation sensitivity ----------------------------------------------

test_that("imputation_sensitivity() refits across strategies and pools them", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  d$wt[c(5, 15)] <- NA
  res <- imputation_sensitivity(
    d,
    fit_fn = function(x) stats::lm(mpg ~ wt + hp, data = x),
    strategies = list(median = "median", mean = "mean", complete_cases = "complete_cases")
  )
  expect_s3_class(res, "imputation_sensitivity")
  expect_equal(res$strategies, c("median", "mean", "complete_cases"))
  expect_equal(res$n_imputed$n_imputed[res$n_imputed$strategy == "median"], 5)
  expect_equal(res$n_imputed$n_dropped[res$n_imputed$strategy == "complete_cases"], 5)
  expect_equal(nrow(res$estimates), 9) # 3 terms x 3 strategies
  expect_equal(sort(unique(res$summary$term)), c("(Intercept)", "hp", "wt"))
  expect_true(all(res$summary$n_strategies == 3))
  expect_true(all(res$summary$range >= 0))
  expect_null(res$pooled_note)
  expect_true(all(c("wt", "hp") %in% res$pooled$term))
  # The wt estimate moves across strategies: the range is the sensitivity signal.
  expect_gt(res$summary$range[res$summary$term == "wt"], 0)
})

test_that("imputation_sensitivity() accepts data-frame and function strategies", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  filled_zero <- d
  filled_zero$hp[is.na(filled_zero$hp)] <- 0
  res <- imputation_sensitivity(
    d,
    fit_fn = function(x) stats::lm(mpg ~ hp, data = x),
    strategies = list(zero = filled_zero, median = "median")
  )
  expect_equal(nrow(res$estimates), 4) # 2 terms x 2 strategies
  expect_equal(res$n_imputed$n_imputed[res$n_imputed$strategy == "zero"], 3)
})

test_that("imputation_sensitivity() validates strategies and fit failures", {
  d <- mtcars
  d$hp[c(3, 11)] <- NA
  fit <- function(x) stats::lm(mpg ~ wt + hp, data = x)
  expect_error(
    imputation_sensitivity(d, fit, strategies = list(median = "median")),
    "at least two"
  )
  expect_error(
    imputation_sensitivity(d, fit, strategies = list(a = "median", b = "nope")),
    "unknown built-in strategy"
  )
  expect_error(
    imputation_sensitivity(
      d, function(x) stop("boom"),
      strategies = list(median = "median", mean = "mean")
    ),
    "fit_fn.*failed: boom"
  )
  expect_error(
    imputation_sensitivity(d, fit, strategies = list(median = "median", mean = "mean"),
                            conf_level = 2),
    "conf_level"
  )
})

test_that("the stored SAS 9.4 LIFETEST listing parses and agrees with R (B5, Tier 1)", {
  lst <- system.file("sas", "list", "benchmark_lifetest_km.lst", package = "TempleCBE")
  skip_if(!nzchar(lst), "stored SAS listing not installed")
  sas <- parse_lifetest_lst(lst)
  # Real SAS output: eight event times in two strata, both medians, the log-rank test.
  expect_equal(nrow(sas[sas$quantity == "km_surv", ]), 8)
  expect_setequal(sas$term[sas$quantity == "median"], c("A", "B"))
  # stratum B: survival is exactly 0.5 from t = 7 to t = 9, SAS prints the midpoint
  expect_equal(sas$value[sas$quantity == "median" & sas$term == "B"], 8)
  r <- km_lifetest_quantities(lifetest_benchmark_data(), "time", "status", group = "group")
  # SAS prints 4 decimals
  cmp <- compare_km_lifetest(r, sas, atol = 1e-4, rtol = 1e-3)
  expect_true(cmp$matched)
})
