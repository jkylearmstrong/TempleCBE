# Regression tests for the 2026-11-12 round-4 review of PR #93 (0.9.0).
# One block per finding id (DA = data/utilities, DC = SAS compare,
# RP = reporting/rendering, X = cross-cutting).

# File-local helpers (same convention as test-round3-gaps.R: shared within
# this file only, not across test files).
docs_root <- function() file.path(test_path(), "..", "..")

# mtcars with the mpg values in rows 3 and 11 missing.
mpg_na_data <- function() {
  d <- mtcars
  d$mpg[c(3, 11)] <- NA
  d
}

# Skip unless a render engine (quarto or rmarkdown) is available.
skip_if_no_render_engine <- function() {
  skip_if_not(requireNamespace("quarto", quietly = TRUE) ||
                requireNamespace("rmarkdown", quietly = TRUE))
}

# --- DA-03 / DA-04 / DA-06 / DA-07 (imputation_sensitivity) --------------------

test_that("imputation_sensitivity() survives infinite observed cells (DA-03)", {
  d <- mpg_na_data()
  d$hp[2] <- Inf
  fit_fn <- function(x) {
    ok <- is.finite(x$hp)
    stats::lm(mpg ~ wt, data = x[ok, , drop = FALSE])
  }
  res <- imputation_sensitivity(d, fit_fn, list(median = "median", mean = "mean"))
  expect_equal(res$n_imputed$n_imputed, c(2L, 2L))
})

test_that("a strategy that adds rows never reports negative n_dropped (DA-06)", {
  d <- mpg_na_data()
  grow <- function(x) {
    x <- rbind(x, x[1:2, ])
    x$mpg[is.na(x$mpg)] <- 0
    x
  }
  res <- imputation_sensitivity(d, function(x) stats::lm(mpg ~ wt, data = x),
                                list(median = "median", grow = grow))
  expect_true(all(res$n_imputed$n_dropped >= 0L))
})

test_that("conf_level = NA gives the intended error (DA-07)", {
  d <- mtcars
  d$mpg[3] <- NA
  expect_error(
    imputation_sensitivity(d, function(x) stats::lm(mpg ~ wt, data = x),
                           list(a = "median", b = "mean"), conf_level = NA_real_),
    "must be in [(]0, 1[)]"
  )
  expect_error(
    km_lifetest_quantities(data.frame(t = 1:5, s = c(1, 0, 1, 1, 0)), "t", "s", conf_level = NA_real_),
    "must be in [(]0, 1[)]"
  )
})

# --- DA-01 / DA-02 (export_to_python) ------------------------------------------

test_that("a list-column dictionary fails before any file is written (DA-01)", {
  dir <- withr::local_tempdir()
  dict <- tibble::tibble(a = 1:2)
  dict$b <- list(1, 2)
  expect_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict),
    "dictionary"
  )
  expect_length(list.files(dir), 0L)
})

test_that("a list element named 'dictionary' cannot collide with the dictionary argument (DA-02)", {
  dir <- withr::local_tempdir()
  expect_error(
    export_to_python(list(dictionary = head(mtcars, 3)), path = dir, name = "n",
                     dictionary = data.frame(term = "a", def = "b")),
    "element named 'dictionary'"
  )
  expect_length(list.files(dir), 0L)
})

# --- DA-05 / DC-01 (SAS compare functions) ---------------------------------------

test_that("compare_km_lifetest() requires numeric value and time (DA-05)", {
  tbl <- data.frame(quantity = "surv", term = "all", time = 1, value = 0.5, stringsAsFactors = FALSE)
  bad <- tbl
  bad$value <- "0.5"
  expect_error(compare_km_lifetest(bad, tbl), "numeric")
  expect_error(compare_km_lifetest(tbl, bad), "numeric")
})

test_that("compare_recurrent_phreg() refuses empty tables (DC-01)", {
  lst <- system.file("sas", "list", "benchmark_recurrent_phreg.lst", package = "TempleCBE")
  skip_if(!nzchar(lst))
  sas <- parse_recurrent_phreg_lst(lst)
  r <- recurrent_phreg_quantities(cgd_recurrent_data())
  expect_error(compare_recurrent_phreg(r[0, ], sas[0, ]), "at least one row")
  expect_error(compare_recurrent_phreg(r[0, ], sas), "at least one row")
  expect_error(compare_recurrent_phreg(r, sas[0, ]), "at least one row")
})

# --- X-01 / X-02 ---------------------------------------------------------------------

test_that("predict_event_risk() validates the horizon before dispatch (X-01)", {
  for (h in list(Inf, -5, 0, NA_real_, c(1, 2), "x")) {
    expect_error(predict_event_risk(structure(list(), class = "cox_time_varying"),
                                    data.frame(a = NA_real_), horizon = h),
                 "single finite, positive")
  }
})

test_that(".safe_spearman_cor() rejects an unknown method (X-02)", {
  expect_error(.safe_spearman_cor(1:5, c(2, 1, 4, 3, 5), method = "bogus"))
  expect_equal(.safe_spearman_cor(1:5, 1:5), 1)
  expect_equal(.safe_spearman_cor(1:5, 1:5, method = "kendall"), 1)
})

# --- RP-02 / RP-03 / RP-04 / RP-05 ---------------------------------------------------

test_that("zip_reports() shows an empty path as <empty> (RP-04)", {
  reports <- data.frame(name = c("a", "b"), path = c("", "b.qmd"), stringsAsFactors = FALSE)
  expect_error(zip_reports(reports, tempfile(fileext = ".zip")), "<empty> [(]row 1[)]")
})

test_that("print() of a review extract labels an unnamed fork and omits a missing master (RP-05)", {
  x <- structure(
    list(
      paths = list(master = NULL, forks = list("f1.xlsx", "f2.xlsx")),
      comments = data.frame(), revisions = data.frame(), redlines = data.frame(),
      errors = data.frame()
    ),
    class = c("docx_review_extract", "list")
  )
  out <- capture.output(print(x))
  expect_false(any(grepl("Master Tracker", out)))
  expect_true(any(grepl("Reviewer Fork (unnamed): f1.xlsx", out, fixed = TRUE)))
  expect_true(any(grepl("Reviewer Fork (unnamed): f2.xlsx", out, fixed = TRUE)))
})

test_that("render_pipeline() does not warn about execute_params on a dry run (RP-01)", {
  skip_if_no_render_engine()
  d <- withr::local_tempdir()
  q <- file.path(d, "doc.Rmd")
  writeLines(c("---", "title: t", "---", "hi"), q)
  expect_no_warning(
    render_pipeline(q, formats = "html", dry_run = TRUE, renv_guard = FALSE,
                    execute_params = list(a = 1))
  )
})

test_that("the SAS compare functions reject a non-data-frame or a table missing columns (Y-07)", {
  tbl <- data.frame(quantity = "surv", term = "all", time = 1, value = 0.5, stringsAsFactors = FALSE)
  expect_error(compare_km_lifetest(list(), tbl), "must be a data frame with columns")
  expect_error(compare_km_lifetest(tbl, tbl[, c("quantity", "term")]), "must be a data frame with columns")
  expect_error(compare_recurrent_phreg(tbl, tbl), "must be a data frame with columns")
})

test_that("a dictionary built with I(list()) is rejected, not flattened (DA-01)", {
  dir <- withr::local_tempdir()
  dict <- data.frame(term = c("a", "b"))
  dict$levels <- I(list(1:2, 3:4))
  expect_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict),
    "list-columns"
  )
  expect_length(list.files(dir), 0L)
})

test_that("a dot named 'NA' is reported as unnamed, not as a phantom argument (RP-02)", {
  skip_if_no_render_engine()
  d <- withr::local_tempdir()
  q <- file.path(d, "doc.Rmd")
  writeLines(c("---", "title: t", "---", "hi"), q)
  args <- list(q, formats = "html", dry_run = TRUE, renv_guard = FALSE)
  args[["NA"]] <- 123
  expect_warning(do.call(render_pipeline, args), "unnamed")
})

test_that("an observed numeric change just under or over 1e-8 is classified as documented (DA-04)", {
  d <- mpg_na_data()
  fit_fn <- function(x) stats::lm(mpg ~ wt, data = x)
  scale_by <- function(k) {
    function(x) {
      x$hp <- x$hp * k
      x$mpg[is.na(x$mpg)] <- 20
      x
    }
  }
  within_tol <- imputation_sensitivity(d, fit_fn, list(a = "median", s = scale_by(1 + 1e-10)))
  outside_tol <- imputation_sensitivity(d, fit_fn, list(a = "median", s = scale_by(1 + 1e-6)))
  expect_equal(within_tol$n_imputed$n_imputed[2], 2L)
  expect_true(is.na(outside_tol$n_imputed$n_imputed[2]))
})

test_that("the reticulate vignette does not call reticulate the reverse direction (ST-05)", {
  vig <- file.path(docs_root(), "vignettes", "reticulate_export.Rmd")
  skip_if_not(file.exists(vig), "vignettes are not in the built package")
  text <- paste(readLines(vig, warn = FALSE), collapse = "\n")
  expect_false(grepl("reverse direction, use `reticulate`", text, fixed = TRUE))
  expect_true(grepl("use `rpy2` directly", text, fixed = TRUE))
})
