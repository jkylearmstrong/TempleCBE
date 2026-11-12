# Regression tests for the 2026-11-12 round-3 review of PR #92 (0.9.0).
# One block per finding id (S = survival core, D = data/utilities,
# R = reporting/rendering, A = SAS baseline, X/M = docs, Y = style).

# --- S-01 ----------------------------------------------------------------------

test_that("recurrent_cox() rejects an NA conf_level with the intended message (S-01)", {
  skip_if_not_installed("survival")
  expect_error(
    recurrent_cox(data.frame(a = 1), ~ a, conf_level = NA_real_),
    "between 0 and 1"
  )
})

# --- S-02 ----------------------------------------------------------------------

test_that(".safe_spearman_cor() is silently NA for a constant vector, both methods (S-02)", {
  x <- rep(0.2, 4)
  y <- c(0.1, 0.2, 0.3, 0.4)
  for (m in c("spearman", "kendall")) {
    expect_no_warning(r <- .safe_spearman_cor(x, y, method = m))
    expect_true(is.na(r))
  }
  expect_equal(.safe_spearman_cor(y, y, method = "kendall"), 1)
})

# --- D-01 / D-02 / D-06 / D-08 (export_to_python) --------------------------------

test_that("export_to_python() fails before writing when a CSV cannot be written (D-01)", {
  dir <- file.path(tempdir(), "r3_d01")
  unlink(dir, recursive = TRUE)
  lc <- tibble::tibble(a = 1:3, b = list(1:2, 3, 4:5))
  expect_error(
    export_to_python(list(lc_df = lc, v = 1:3), path = dir, name = "lc"),
    "Cannot export element `lc_df` as CSV"
  )
  expect_length(list.files(dir), 0L)
  expect_error(export_to_python(lc, path = dir, name = "lc2"), "Cannot export `object` as CSV")
  expect_length(list.files(dir), 0L)
})

test_that("export_to_python() validates list element names (D-02)", {
  dir <- file.path(tempdir(), "r3_d02")
  unlink(dir, recursive = TRUE)
  expect_error(
    export_to_python(list(`a/b` = head(mtcars, 3)), path = dir, name = "n"),
    "unique, not missing, and free of path separators"
  )
  dup <- list(x = head(mtcars, 3), x = head(iris, 3))
  expect_error(export_to_python(dup, path = dir, name = "n"), "unique")
  expect_length(list.files(dir), 0L)
})

test_that("export_to_python() rejects empty, duplicate and unsorted eval_time up front (D-06)", {
  skip_if_not_installed("glmnet")
  d <- survival::lung
  d <- d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
  d$status <- d$status - 1
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
  dir <- file.path(tempdir(), "r3_d06")
  for (et in list(numeric(0), c(100, 100), c(200, 100))) {
    expect_error(
      export_to_python(fit, path = dir, new_data = d[1:3, ], eval_time = et),
      "eval_time"
    )
  }
  expect_false(dir.exists(dir) && length(list.files(dir)) > 0L)
})

test_that("the overwrite guard sees a previous coefficients file (D-08)", {
  dir <- file.path(tempdir(), "r3_d08")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  file.create(file.path(dir, "n_coefficients.csv"))
  expect_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n"),
    "already exists"
  )
})

# --- D-03 / D-04 (tune_impute_workflows) -----------------------------------------

test_that("tune_impute_workflows() accepts a rolling_origin rset (D-03)", {
  for (p in c("workflowsets", "tune", "rsample", "parsnip", "workflows", "yardstick", "recipes")) {
    skip_if_not_installed(p)
  }
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  train <- d[!is.na(d$mpg), , drop = FALSE]
  rset <- rsample::rolling_origin(train, initial = 20, assess = 3, cumulative = TRUE)
  err <-tryCatch(
    tune_impute_workflows(d, "mpg", folds = rset, race = FALSE, grid = 1),
    error = function(e) conditionMessage(e)
  )
  expect_false(is.character(err), info = if (is.character(err)) err else "")
})

test_that("tune_impute_workflows() rejects a split whose analysis and assessment rows overlap (D-03)", {
  for (p in c("workflowsets", "tune", "rsample", "parsnip", "workflows", "yardstick", "recipes")) {
    skip_if_not_installed(p)
  }
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  train <- d[!is.na(d$mpg), , drop = FALSE] # 29 rows
  split <- rsample::make_splits(list(analysis = 1:25, assessment = 21:24), data = train)
  rset <- rsample::manual_rset(list(split), "Fold1")
  expect_error(
    tune_impute_workflows(d, "mpg", folds = rset, race = FALSE, grid = 1),
    "overlap"
  )
})

test_that("seed must be finite and within the integer range (D-04)", {
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  for (s in list(Inf, -Inf, 1e10)) {
    expect_error(tune_impute_dataset(d, seed = s), "single whole number")
  }
})

# --- D-05 (n_imputed alignment) ---------------------------------------------------

test_that("n_imputed aligns on observed cells, not on row names or class (D-05)", {
  d <- mtcars
  d$hp[c(3, 11, 20)] <- NA
  fit_fn <- function(x) stats::lm(mpg ~ hp, data = x)
  n_for <- function(data, strat) {
    res <- suppressWarnings(imputation_sensitivity(
      data, fit_fn = fit_fn, strategies = list(median = "median", s = strat)
    ))
    res$n_imputed$n_imputed[res$n_imputed$strategy == "s"]
  }
  # same order, values filled, row names reset: the count is still 3
  expect_equal(n_for(d, function(x) {
    x$hp[is.na(x$hp)] <- 100
    rownames(x) <- NULL
    x
  }), 3L)
  # tibble, reversed, one of three NAs filled: cannot be aligned -> NA
  expect_true(is.na(n_for(tibble::as_tibble(d), function(x) {
    x <- x[rev(seq_len(nrow(x))), ]
    x$hp[which(is.na(x$hp))[1]] <- 100
    x
  })))
  # data frame, reversed, row names reset, filled: NA, not a positional guess
  expect_true(is.na(n_for(d, function(x) {
    x <- x[rev(seq_len(nrow(x))), ]
    x$hp[is.na(x$hp)] <- 100
    rownames(x) <- NULL
    x
  })))
  # pure reordering still counts nothing
  expect_equal(n_for(tibble::as_tibble(d), function(x) x[rev(seq_len(nrow(x))), ]), 0L)
})

# --- D-07 (compare_km_lifetest validation) ----------------------------------------

test_that("compare_km_lifetest() validates tolerances and empty inputs (D-07)", {
  tbl <- data.frame(quantity = "km_surv", term = "A", time = 5, value = 0.5)
  expect_error(compare_km_lifetest(tbl, tbl, atol = NA_real_), "finite, non-negative")
  expect_error(compare_km_lifetest(tbl, tbl, atol = Inf), "finite, non-negative")
  expect_error(compare_km_lifetest(tbl, tbl, rtol = Inf), "finite, non-negative")
  expect_error(compare_km_lifetest(tbl[0, ], tbl[0, ]), "at least one row")
  expect_true(compare_km_lifetest(tbl, tbl)$matched)
})

# --- R-01 / R-06 (horizon risk) ----------------------------------------------------

test_that("extract_horizon_risk() rejects a non-finite horizon and names .pred_cif (R-01, Y-04)", {
  curve <- tibble::tibble(.eval_time = c(5, 10), .pred_survival = c(0.9, 0.8))
  pr <- tibble::tibble(.pred = list(curve, curve))
  expect_error(extract_horizon_risk(pr, Inf), "finite")
  expect_equal(extract_horizon_risk(pr, 5), c(0.1, 0.1))
  expect_error(extract_horizon_risk(tibble::tibble(x = 1), 5), "\\.pred_cif")
})

test_that("predict_event_risk() gives NA risk to cox_time_varying rows with missing predictors (R-06)", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d <- d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
  d$status <- d$status - 1
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ sex + age, tv_vars = "age")
  nd <- d[1:4, ]
  nd$age[2] <- NA
  risk <- predict_event_risk(fit, nd, horizon = 200)
  expect_length(risk, 4L)
  expect_true(is.na(risk[2]))
  expect_false(anyNA(risk[-2]))
})

# --- R-02 (named unsupported dots) -------------------------------------------------

test_that("render_pipeline() warns about named dots no engine accepts, even on a dry run (R-02)", {
  skip_if_not_installed("quarto")
  d <- file.path(tempdir(), "r3_r02")
  dir.create(d, showWarnings = FALSE)
  q <- file.path(d, "doc.qmd")
  writeLines(c("---", "title: t", "format: html", "---", "hi"), q)
  expect_warning(
    render_pipeline(q, formats = "html", dry_run = TRUE, renv_guard = FALSE, bogus_named = 123),
    "bogus_named"
  )
  expect_no_warning(
    render_pipeline(q, formats = "html", dry_run = TRUE, renv_guard = FALSE, quiet = TRUE)
  )
})

# --- R-03 / R-07 (.ask_item) --------------------------------------------------------

test_that(".ask_item() reports numeric-looking non-integers as bad item numbers (R-03)", {
  for (ans in c("2.5", "-1", "+2", "1e2")) {
    local({
      a <- ans
      testthat::local_mocked_bindings(.read_answer = function(prompt) a, .package = "TempleCBE")
      expect_error(.ask_item(c("a", "b"), "?"), "No item number")
    })
  }
})

test_that(".ask_item() lets a number win over a plan name that looks like one (R-07)", {
  testthat::local_mocked_bindings(.read_answer = function(prompt) "2", .package = "TempleCBE")
  expect_equal(.ask_item(c("2", "b"), "?"), 2L)
})

# --- R-04 (zip_reports NA path) -----------------------------------------------------

test_that("zip_reports() names the row of an NA path (R-04)", {
  reports <- data.frame(name = c("a", "b"), path = c("a.qmd", NA_character_), stringsAsFactors = FALSE)
  expect_error(zip_reports(reports, tempfile(fileext = ".zip")), "<NA> \\(row 2\\)")
})

# --- R-05 (print of a combined extract) ----------------------------------------------

test_that("print.docx_review_extract() keeps multi-path entries on one line (R-05)", {
  x <- structure(
    list(
      paths = list(master = c("m1.xlsx", "m2.xlsx"), forks = list(), comments_csv = c("c1.csv", "c2.csv")),
      comments = data.frame(), revisions = data.frame(), redlines = data.frame(),
      errors = data.frame()
    ),
    class = "docx_review_extract"
  )
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("m1.xlsx, m2.xlsx", out, fixed = TRUE)))
  expect_true(any(grepl("c1.csv, c2.csv", out, fixed = TRUE)))
})

# --- A-02 / A-03 (recurrent PHREG parser and tolerance) -------------------------------

test_that("the listing parser drops a stray non-ASCII byte inside a number (A-02)", {
  lst <- system.file("sas", "list", "benchmark_recurrent_phreg.lst", package = "TempleCBE")
  expect_true(nzchar(lst))
  lines <- readLines(lst, warn = FALSE)
  clean <- parse_recurrent_phreg_lst(lines)
  hit <- grep("-1.11911", lines, fixed = TRUE)[1]
  expect_false(is.na(hit))
  dirty <- lines
  dirty[hit] <- sub("-1.11911", paste0("-1.1", rawToChar(as.raw(0x83)), "1911"), dirty[hit], fixed = TRUE, useBytes = TRUE)
  expect_equal(parse_recurrent_phreg_lst(dirty), clean)
})

test_that("compare_recurrent_phreg() does not let a relative allowance hide a 1e-4 estimate shift (A-03)", {
  lst <- system.file("sas", "list", "benchmark_recurrent_phreg.lst", package = "TempleCBE")
  skip_if(!nzchar(lst))
  sas <- parse_recurrent_phreg_lst(lst)
  r <- recurrent_phreg_quantities(cgd_recurrent_data())
  expect_equal(formals(compare_recurrent_phreg)$rtol, 0)
  expect_true(compare_recurrent_phreg(r, sas)$matched)
  r_bad <- r
  r_bad$estimate[r_bad$model == "ag" & r_bad$term == "treat"] <-
    r_bad$estimate[r_bad$model == "ag" & r_bad$term == "treat"] + 1e-4
  expect_false(compare_recurrent_phreg(r_bad, sas)$matched)
  expect_error(compare_recurrent_phreg(r, sas, atol = NA_real_), "finite, non-negative")
})

# --- X-01 / X-02 (docs consistency) ----------------------------------------------------

test_that("NEWS no longer says the recurrent PHREG program is drafted only (X-01)", {
  news <- file.path(test_path(), "..", "..", "NEWS.md")
  skip_if_not(file.exists(news), "NEWS.md is not in the built package")
  text <- paste(readLines(news, warn = FALSE), collapse = "\n")
  expect_false(grepl("is drafted only", text, fixed = TRUE))
  expect_false(grepl("skips until the listing exists", text, fixed = TRUE))
})

test_that("the reticulate vignette does not call the export bidirectional (X-02)", {
  vig <- file.path(test_path(), "..", "..", "vignettes", "reticulate_export.Rmd")
  skip_if_not(file.exists(vig), "vignettes are not in the built package")
  text <- paste(readLines(vig, warn = FALSE), collapse = "\n")
  expect_false(grepl("idirectional", text, fixed = TRUE))
})
