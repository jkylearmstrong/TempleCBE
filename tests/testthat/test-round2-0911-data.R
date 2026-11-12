# Round-2 fresh-review fixes, data/utilities items 11-21 (PR #91 head 57cfda3).
# Each test names the item it covers. Tests needing Suggests packages that
# are absent here (workflowsets, missForest, ...) skip; CI exercises them.

r2_lifetest_data <- function() {
  data.frame(
    time = c(5L, 6L, 6L, 8L, 10L, 12L, 3L, 4L, 7L, 9L, 9L, 11L),
    status = c(1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L),
    group = rep(c("A", "B"), each = 6)
  )
}

r2_lifetest_listing <- function() {
  c(
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
}

r2_competing_data <- function(seed = 1, n = 150) {
  set.seed(seed)
  data.frame(
    time = stats::rexp(n, 0.1),
    type = sample(0:2, n, TRUE, prob = c(0.3, 0.4, 0.3)),
    age = stats::rnorm(n, 60, 8), arm = rep(c("A", "B"), length.out = n)
  )
}

# --- Item 11: classed model objects get the documented error, not jsonlite's ----

test_that("export_to_python() rejects the package's own new model classes clearly (item 11)", {
  skip_if_not_installed("cmprsk")
  dir <- file.path(tempdir(), "pyexp_r2_fg")
  fit <- fine_gray(r2_competing_data(), "time", "type", ~ age + arm)
  expect_s3_class(fit, "fine_gray_model")
  expect_error(
    export_to_python(fit, path = dir, name = "fg"),
    "must be a data frame, matrix"
  )
  # the rejection happens before anything is written: no directory at all
  expect_false(dir.exists(dir))
  # the other two classed-list model classes take the same path
  for (cl in c("cox_time_varying", "joint_readmission_death")) {
    fake <- structure(list(x = 1), class = cl)
    expect_error(
      export_to_python(fake, path = file.path(tempdir(), paste0("pyexp_r2_", cl)), name = "m"),
      "must be a data frame, matrix"
    )
  }
  # ... while a genuinely unsupported object keeps the same message
  expect_error(export_to_python(mean, path = dir, name = "m"), "must be a data frame, matrix")
})

test_that("export_to_python() leaves no partial export behind for an unserialisable list (item 11)", {
  dir <- file.path(tempdir(), "pyexp_r2_partial")
  bad <- list(
    df = head(mtcars, 5),
    model = structure(list(x = 1), class = "fine_gray_model")
  )
  expect_error(
    export_to_python(bad, path = dir, name = "p"),
    "written as JSON"
  )
  expect_equal(length(list.files(dir)), 0L)
  # a plain named list of exportable values still works
  out <- export_to_python(
    list(df = head(mtcars, 5), v = 1:3), path = dir, name = "ok", overwrite = TRUE
  )
  expect_true(file.exists(file.path(dir, "ok_df.csv")))
  expect_true(file.exists(file.path(dir, "ok.json")))
  expect_true(file.exists(file.path(dir, "ok_manifest.json")))
})

# --- Item 12: joint_model survival matrix --------------------------------------

test_that("export_to_python() exports a survival matrix for joint_model (item 12)", {
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
  dir <- file.path(tempdir(), "pyexp_r2_jm")
  out <- export_to_python(fit, path = dir, name = "jm", new_data = df[1:3, ], eval_time = c(5, 10))
  expect_true(file.exists(file.path(dir, "jm_survival.csv")))
  surv <- utils::read.csv(file.path(dir, "jm_survival.csv"))
  expect_equal(nrow(surv), 3)
  expect_true(all(c("t_5", "t_10") %in% names(surv)))
  expect_true(all(surv$t_5 >= 0 & surv$t_5 <= 1))
  # the exported matrix is predict()'s own .pred_survival curves
  pred <- stats::predict(fit, new_data = df[1:3, ], type = "survival", eval_time = c(5, 10))
  at <- function(cv, t) cv$.pred_survival[cv$.eval_time == t]
  expect_equal(surv$t_5, vapply(pred$.pred_survival, at, numeric(1), t = 5))
  expect_equal(surv$t_10, vapply(pred$.pred_survival, at, numeric(1), t = 10))
})

# --- Item 13: compare_km_lifetest() defaults accept the stored benchmark ------

test_that("compare_km_lifetest() defaults accept the SAS benchmark (item 13)", {
  expect_equal(formals(compare_km_lifetest)$atol, 6e-5)
  r <- km_lifetest_quantities(r2_lifetest_data(), "time", "status", group = "group")
  sas <- parse_lifetest_lst(r2_lifetest_listing())
  cmp <- compare_km_lifetest(r, sas)
  expect_true(cmp$matched)
  # a genuinely different value still fails under the new defaults
  sas_bad <- sas
  sas_bad$value[sas_bad$quantity == "km_surv" & sas_bad$term == "A" & sas_bad$time == 5] <- 0.5
  expect_false(compare_km_lifetest(r, sas_bad)$matched)
})

test_that("compare_km_lifetest() defaults accept the stored SAS 9.4 listing (item 13)", {
  lst <- system.file("sas", "list", "benchmark_lifetest_km.lst", package = "TempleCBE")
  skip_if(!nzchar(lst), "stored SAS listing not installed")
  r <- km_lifetest_quantities(r2_lifetest_data(), "time", "status", group = "group")
  cmp <- compare_km_lifetest(r, parse_lifetest_lst(lst))
  expect_true(cmp$matched)
})

# --- Item 14: agreement when the median is not reached on both sides ---------

test_that("compare_km_lifetest() matches a median that is NA on both sides (item 14)", {
  mk <- function(median_value) {
    tibble::tibble(
      quantity = c("km_surv", "median"), term = c("A", "A"),
      time = c(5, NA_real_), value = c(0.8, median_value)
    )
  }
  cmp <- compare_km_lifetest(mk(NA_real_), mk(NA_real_))
  expect_true(cmp$table$matched[cmp$table$quantity == "median"])
  expect_true(cmp$matched)
  # ... but a median reached on one side only is still a mismatch
  cmp2 <- compare_km_lifetest(mk(NA_real_), mk(8))
  expect_false(cmp2$table$matched[cmp2$table$quantity == "median"])
  expect_false(cmp2$matched)
})

test_that("compare_km_lifetest() reports NA, not -Inf, when every diff is NA (item 14)", {
  mk <- function() {
    tibble::tibble(
      quantity = c("median", "median"), term = c("A", "B"),
      time = c(NA_real_, NA_real_), value = c(NA_real_, NA_real_)
    )
  }
  expect_no_warning(cmp <- compare_km_lifetest(mk(), mk()))
  expect_true(cmp$matched)
  expect_true(is.na(cmp$max_abs_diff))
})

# --- Item 15: 0-row input -------------------------------------------------------

test_that("km_lifetest_quantities() stops on 0-row input (item 15)", {
  d <- r2_lifetest_data()[0, ]
  expect_error(km_lifetest_quantities(d, "time", "status", group = "group"), "at least one row")
})

# --- Item 16: all-NA term summary is NA, not Inf/-Inf --------------------------

test_that("imputation_sensitivity() summary is NA for an all-NA term (item 16)", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  d$wt[c(5, 15)] <- NA
  d$hp2 <- d$hp # exactly collinear with hp: its estimate is NA under every strategy
  res <- expect_no_warning(imputation_sensitivity(
    d,
    fit_fn = function(x) stats::lm(mpg ~ wt + hp + hp2, data = x),
    strategies = list(median = "median", mean = "mean", complete_cases = "complete_cases")
  ))
  row <- res$summary[res$summary$term == "hp2", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_true(is.na(row$min))
  expect_true(is.na(row$max))
  expect_true(is.na(row$range))
  expect_true(is.na(row$sd))
  expect_true(is.na(row$median_se))
  # the other terms still summarise, and pooling correctly refuses
  expect_true(all(is.finite(res$summary$range[res$summary$term == "wt"])))
  expect_false(is.null(res$pooled_note))
})

# --- Item 17: n_imputed is not positional --------------------------------------

test_that("imputation_sensitivity() does not count a pure row reordering as imputed (item 17)", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  # the 'reversed' strategy leaves the NAs in place, which the function
  # warns about; that warning is expected here, not part of the assertion
  res <- suppressWarnings(imputation_sensitivity(
    d,
    fit_fn = function(x) stats::lm(mpg ~ hp, data = x),
    strategies = list(
      median = "median",
      reversed = function(x) x[rev(seq_len(nrow(x))), ]
    )
  ))
  expect_equal(res$n_imputed$n_imputed[res$n_imputed$strategy == "median"], 3)
  expect_equal(res$n_imputed$n_imputed[res$n_imputed$strategy == "reversed"], 0)
})

test_that("imputation_sensitivity() reports NA when rows are reordered and changed (item 17)", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  res <- suppressWarnings(imputation_sensitivity(
    d,
    fit_fn = function(x) stats::lm(mpg ~ hp, data = x),
    strategies = list(
      median = "median",
      reversed_filled = function(x) {
        out <- x[rev(seq_len(nrow(x))), ]
        out$hp[is.na(out$hp)] <- stats::median(out$hp, na.rm = TRUE)
        out
      }
    )
  ))
  expect_true(is.na(res$n_imputed$n_imputed[res$n_imputed$strategy == "reversed_filled"]))
  # the same pure-reordering case on a tibble (no row names to betray it)
  dt <- tibble::as_tibble(d)
  res_tib <- suppressWarnings(imputation_sensitivity(
    dt,
    fit_fn = function(x) stats::lm(mpg ~ hp, data = x),
    strategies = list(
      median = "median",
      reversed = function(x) x[rev(seq_len(nrow(x))), ]
    )
  ))
  expect_equal(res_tib$n_imputed$n_imputed[res_tib$n_imputed$strategy == "median"], 3)
  expect_equal(res_tib$n_imputed$n_imputed[res_tib$n_imputed$strategy == "reversed"], 0)
})

# --- Item 18: tune_impute_dataset() validates seed -----------------------------

test_that("tune_impute_dataset() validates seed up front (item 18)", {
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  expect_error(tune_impute_dataset(d, seed = "abc"), "single whole number")
  expect_error(tune_impute_dataset(d, seed = c(1, 2)), "single whole number")
  expect_error(tune_impute_dataset(d, seed = NA), "single whole number")
  expect_error(tune_impute_dataset(d, seed = 1.5), "single whole number")
  # the forest engines get the same up-front check
  expect_error(tune_impute_dataset(d, engine = "missforest", seed = "abc"), "single whole number")
})

# --- Item 19: per-split size check for user-supplied rsets --------------------

test_that("tune_impute_workflows() rejects an rset that does not cover the observed rows (item 19)", {
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("tune")
  skip_if_not_installed("rsample")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("recipes")
  d <- mtcars
  d$mpg[c(30, 31, 32)] <- NA # 29 observed rows
  # a manual split covering only rows 1:25, all with observed mpg and
  # in-range indices: the old heuristic checks pass it, the size check must not
  split <- rsample::make_splits(list(analysis = 1:20, assessment = 21:25), data = d)
  rset <- rsample::manual_rset(list(split), "Fold1")
  expect_error(
    tune_impute_workflows(d, "mpg", folds = rset, race = FALSE, grid = 1),
    "covers 25 rows, not 29"
  )
  # a full-data vfold is still rejected
  expect_error(
    tune_impute_workflows(d, "mpg", folds = rsample::vfold_cv(d, v = 3), race = FALSE, grid = 1),
    "must be built on the rows"
  )
})

test_that("tune_impute_workflows() accepts an rset built on the observed rows (item 19)", {
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("tune")
  skip_if_not_installed("rsample")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("recipes")
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  train <- d[!is.na(d$mpg), , drop = FALSE]
  split <- rsample::make_splits(list(analysis = 1:20, assessment = 21:29), data = train)
  rset <- rsample::manual_rset(list(split), "Fold1")
  res <- tune_impute_workflows(d, "mpg", folds = rset, race = FALSE, grid = 1)
  expect_s3_class(res, "cbe_impute_tune")
  expect_equal(nrow(res$predictions), 9)
  expect_equal(res$n_imputed, 3)
})

# --- Item 20: manifest lists itself --------------------------------------------

test_that("export_to_python() manifest lists the manifest itself (item 20)", {
  dir <- file.path(tempdir(), "pyexp_r2_man")
  out <- export_to_python(head(mtcars, 10), path = dir, name = "demo")
  manifest <- jsonlite::fromJSON(file.path(dir, "demo_manifest.json"))
  expect_true("demo_manifest.json" %in% manifest$files)
  expect_setequal(manifest$files, basename(out$files))
})

# --- Item 21: eval_time must be finite ------------------------------------------

test_that("export_to_python() rejects non-finite eval_time (item 21)", {
  dir <- file.path(tempdir(), "pyexp_r2_eval")
  expect_error(
    export_to_python(head(mtcars), path = dir, name = "x", eval_time = Inf, new_data = head(mtcars)),
    "finite, non-negative"
  )
  expect_error(
    export_to_python(head(mtcars), path = dir, name = "x", eval_time = c(100, Inf),
                     new_data = head(mtcars)),
    "finite, non-negative"
  )
  expect_error(
    export_to_python(head(mtcars), path = dir, name = "x", eval_time = NA_real_,
                     new_data = head(mtcars)),
    "finite, non-negative"
  )
  expect_error(
    export_to_python(head(mtcars), path = dir, name = "x", eval_time = -1, new_data = head(mtcars)),
    "finite, non-negative"
  )
})
