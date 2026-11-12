# Regression tests for the 2026-11-12 round-5 review of merged master 285b2d4
# (0.9.0). One block per finding id (R5-01 to R5-12), plus the ST-04
# test-hygiene residual. Run sequentially with testthat::test_file().

# --- R5-01 / R5-02 (Python export) -------------------------------------------

test_that("an I(list()) object is rejected, not flattened (R5-01)", {
  dir <- withr::local_tempdir()
  obj <- data.frame(term = c("a", "b"))
  obj$vals <- I(list(1:2, 3:4))
  expect_error(
    export_to_python(obj, path = dir, name = "n"),
    "list-columns"
  )
  expect_length(list.files(dir), 0L)
})

test_that("an I(list()) list element is rejected, not flattened (R5-01)", {
  dir <- withr::local_tempdir()
  el <- data.frame(term = c("a", "b"))
  el$vals <- I(list(1:2, 3:4))
  expect_error(
    export_to_python(list(a = el, b = head(mtcars, 3)), path = dir, name = "n"),
    "list-columns"
  )
  expect_length(list.files(dir), 0L)
})

test_that("a plain list-column object is still rejected (R5-01)", {
  dir <- withr::local_tempdir()
  obj <- tibble::tibble(term = c("a", "b"))
  obj$vals <- list(1:2, 3:4)
  expect_error(
    export_to_python(obj, path = dir, name = "n"),
    "list-column"
  )
  expect_length(list.files(dir), 0L)
})

test_that("a POSIXlt tibble dictionary exports fine (R5-02)", {
  dir <- withr::local_tempdir()
  dict <- tibble::tibble(
    term = c("a", "b"),
    when = as.POSIXlt(c("2026-01-01 10:00:00", "2026-01-02 11:00:00"), tz = "UTC")
  )
  expect_no_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict)
  )
  expect_true(file.exists(file.path(dir, "n_dictionary.csv")))
  csv <- paste(readLines(file.path(dir, "n_dictionary.csv"), warn = FALSE), collapse = "\n")
  expect_true(grepl("2026-01-01 10:00:00", csv, fixed = TRUE))
  expect_true(grepl("2026-01-02 11:00:00", csv, fixed = TRUE))
  expect_length(list.files(dir), 5L)
})

test_that("a nested data-frame tibble dictionary exports fine (R5-02)", {
  dir <- withr::local_tempdir()
  dict <- tibble::tibble(term = c("a", "b"), nested = tibble::tibble(x = 1:2, y = 3:4))
  expect_no_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict)
  )
  expect_true(file.exists(file.path(dir, "n_dictionary.csv")))
  csv <- read.csv(file.path(dir, "n_dictionary.csv"), check.names = FALSE)
  expect_true(all(c("nested.x", "nested.y") %in% names(csv)))
  expect_equal(csv$nested.x, 1:2)
  expect_equal(csv$nested.y, 3:4)
  expect_length(list.files(dir), 5L)
})

test_that("a plain dictionary still exports normally (R5-01/R5-02 control)", {
  dir <- withr::local_tempdir()
  dict <- data.frame(term = c("a", "b"), def = c("first", "second"), stringsAsFactors = FALSE)
  expect_no_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict)
  )
  expect_equal(
    sort(list.files(dir)),
    sort(c("n_data.csv", "n_dictionary.csv", "n_dictionary.json", "n_load.py", "n_manifest.json"))
  )
})

test_that("a lossy column nested inside a nested data frame is rejected too (R5-01)", {
  dir <- withr::local_tempdir()
  dict <- tibble::tibble(
    term = c("a", "b"),
    nested = tibble::tibble(x = 1:2, vals = I(list(1:2, 3:4)))
  )
  expect_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict),
    "list-columns"
  )
  expect_length(list.files(dir), 0L)
})

test_that("a POSIXlt column nested inside a nested data frame still exports (R5-02)", {
  dir <- withr::local_tempdir()
  dict <- tibble::tibble(
    term = c("a", "b"),
    nested = tibble::tibble(
      x = 1:2,
      when = as.POSIXlt(c("2026-01-01 10:00:00", "2026-01-02 11:00:00"), tz = "UTC")
    )
  )
  expect_no_error(
    export_to_python(head(mtcars, 3), path = dir, name = "n", dictionary = dict)
  )
  expect_true(file.exists(file.path(dir, "n_dictionary.csv")))
  expect_length(list.files(dir), 5L)
})

# --- R5-04 / R5-05 / R5-08 (SAS parity) ---------------------------------------

# Shared fixtures for the phreg comparer tests.
phreg_r <- function() {
  tibble::tibble(
    model = "ag", term = "treat",
    estimate = -1.1, std_error = 0.31, std_error_model = 0.30,
    hazard_ratio = exp(-1.1), p_value = 0.0004, p_value_model = 0.0003
  )
}

phreg_sas <- function() {
  tibble::tibble(
    model = "ag", se_type = c("robust", "model"), term = "treat",
    estimate = -1.1, std_error = c(0.31, 0.30),
    p_value = c(0.0004, 0.0003), hazard_ratio = exp(-1.1)
  )
}

test_that("R5-04: compare_recurrent_phreg() errors when sas lacks hazard_ratio", {
  r <- phreg_r()
  sas_no_hr <- dplyr::select(phreg_sas(), -hazard_ratio)
  expect_error(
    compare_recurrent_phreg(r, sas_no_hr),
    "hazard_ratio"
  )
  # Even a deliberately wrong R hazard ratio must not silently match.
  r_wrong <- r
  r_wrong$hazard_ratio <- 0.99
  expect_error(
    compare_recurrent_phreg(r_wrong, sas_no_hr),
    "hazard_ratio"
  )
})

test_that("R5-04: positive control -- sas with hazard_ratio gives 6 rows", {
  out <- compare_recurrent_phreg(phreg_r(), phreg_sas())
  expect_equal(nrow(out$table), 6L)
  expect_true(out$matched)
  expect_true("hazard_ratio" %in% out$table$quantity)
  # With the column present, a wrong R hazard ratio is caught, not matched.
  r_wrong <- phreg_r()
  r_wrong$hazard_ratio <- 0.99
  out_wrong <- compare_recurrent_phreg(r_wrong, phreg_sas())
  expect_equal(nrow(out_wrong$table), 6L)
  expect_false(out_wrong$matched)
  expect_false(out_wrong$table$matched[out_wrong$table$quantity == "hazard_ratio"])
})

test_that("R5-05: compare_km_lifetest() accepts Date time columns", {
  tm <- as.Date(c("2026-01-01", "2026-01-02"))
  r <- tibble::tibble(
    quantity = c("km_surv", "km_surv", "median"), term = "all",
    time = c(tm, as.Date(NA)), value = c(0.9, 0.8, NA_real_)
  )
  sas <- r
  out <- compare_km_lifetest(r, sas)
  expect_true(out$matched)
  expect_equal(nrow(out$table), 3L)
})

test_that("R5-05: compare_km_lifetest() accepts POSIXct time columns", {
  tm <- as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 10:00:00"), tz = "UTC")
  r <- tibble::tibble(
    quantity = c("km_surv", "km_surv"), term = "all",
    time = tm, value = c(0.9, 0.8)
  )
  out <- compare_km_lifetest(r, r)
  expect_true(out$matched)
  expect_equal(nrow(out$table), 2L)
})

test_that("R5-05: compare_km_lifetest() still rejects a Date value column", {
  tm <- as.Date(c("2026-01-01", "2026-01-02"))
  r <- tibble::tibble(
    quantity = c("km_surv", "km_surv"), term = "all",
    time = tm, value = as.Date(c("2026-01-01", "2026-01-02"))
  )
  sas <- tibble::tibble(
    quantity = c("km_surv", "km_surv"), term = "all",
    time = tm, value = c(0.9, 0.8)
  )
  expect_error(compare_km_lifetest(r, sas), "must be numeric: value")
  expect_error(compare_km_lifetest(sas, r), "must be numeric: value")
})

test_that("R5-08: compare_recurrent_phreg() rejects character in any r numeric column", {
  for (col in c("estimate", "std_error", "std_error_model", "hazard_ratio",
                "p_value", "p_value_model")) {
    r_bad <- phreg_r()
    r_bad[[col]] <- as.character(r_bad[[col]])
    expect_error(
      compare_recurrent_phreg(r_bad, phreg_sas()),
      "must be numeric",
      info = paste("r column:", col)
    )
  }
})

test_that("R5-08: compare_recurrent_phreg() rejects character in any sas numeric column", {
  for (col in c("estimate", "std_error", "p_value", "hazard_ratio")) {
    sas_bad <- phreg_sas()
    sas_bad[[col]] <- as.character(sas_bad[[col]])
    expect_error(
      compare_recurrent_phreg(phreg_r(), sas_bad),
      "must be numeric",
      info = paste("sas column:", col)
    )
  }
})

# --- R5-03 / R5-06 / R5-10 (imputation tolerance, horizon validation) ---------

test_that("integer extremes near .Machine$integer.max do not abort alignment (R5-06)", {
  d_max <- data.frame(x = 2147483647L)
  d_min <- data.frame(x = -2147483647L)
  # Opposite extremes differ: FALSE, not an error (and no overflow warning).
  expect_no_warning(res1 <- .observed_cells_aligned(d_min, d_max))
  expect_false(res1)
  expect_no_warning(res2 <- .observed_cells_aligned(d_max, d_min))
  expect_false(res2)
  # max vs max-1 is within the documented tolerance: TRUE.
  expect_true(.observed_cells_aligned(data.frame(x = 2147483646L), d_max))
  # Double extremes and mixed int/double already returned FALSE.
  expect_false(.observed_cells_aligned(data.frame(x = -1e308), data.frame(x = 1e308)))
  expect_false(.observed_cells_aligned(data.frame(x = -2147483647), d_max))
})

test_that("infinite observed cells still align exactly as DA-03 requires (R5-06 control)", {
  expect_true(.observed_cells_aligned(data.frame(x = Inf), data.frame(x = Inf)))
  expect_false(.observed_cells_aligned(data.frame(x = -Inf), data.frame(x = Inf)))
  expect_false(.observed_cells_aligned(data.frame(x = 5), data.frame(x = Inf)))
})

test_that("the documented 1e-8 tolerance boundary holds at 0.5 (R5-03)", {
  d <- data.frame(x = 0.5)
  # +9e-9 is within 1e-8 * max(1, |0.5|) = 1e-8: unchanged.
  expect_true(.observed_cells_aligned(data.frame(x = 0.5 + 9e-9), d))
  # +2e-8 exceeds it: changed.
  expect_false(.observed_cells_aligned(data.frame(x = 0.5 + 2e-8), d))
})

test_that("the horizon error message is identical at every entry point (R5-10)", {
  horizon_error <- function(expr) {
    tryCatch(expr, error = conditionMessage)
  }
  fake <- structure(list(), class = "cox_time_varying")
  nd <- data.frame(time = c(1, 2, 3), status = c(1, 0, 1))
  msg_extract <- horizon_error(extract_horizon_risk(data.frame(), horizon = 0))
  msg_predict <- horizon_error(predict_event_risk(fake, data.frame(a = NA_real_), horizon = 0))
  msg_calib <- horizon_error(calibration_from_model(fake, nd, "time", "status", horizon = 0))
  msg_dc <- horizon_error(decision_curve_from_model(fake, nd, "time", "status", horizon = 0))
  expect_match(msg_extract, "single finite, positive time", fixed = TRUE)
  expect_identical(msg_predict, msg_extract)
  expect_identical(msg_calib, msg_extract)
  expect_identical(msg_dc, msg_extract)
  # Other invalid horizons error the same way at the two direct entry points.
  for (h in list(0L, -5, Inf, -Inf, NA_real_, c(1, 2), "x", TRUE, NULL)) {
    expect_error(extract_horizon_risk(data.frame(), horizon = h), "single finite, positive time")
    expect_error(predict_event_risk(fake, data.frame(a = NA_real_), horizon = h), "single finite, positive time")
  }
})

test_that("a valid horizon still returns risks at every entry point (R5-10)", {
  pred <- tibble::tibble(.pred = list(
    tibble::tibble(.eval_time = c(5, 10, 20), .pred_survival = c(0.9, 0.8, 0.7)),
    tibble::tibble(.eval_time = c(5, 10, 20), .pred_survival = c(0.95, 0.85, 0.75))
  ))
  expect_equal(extract_horizon_risk(pred, 10), c(0.2, 0.15))

  set.seed(1)
  n <- 120
  d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
                  age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
  fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
  risk <- predict_event_risk(fit, d[1:5, ], horizon = 10)
  expect_length(risk, 5L)
  expect_true(all(is.finite(risk) & risk >= 0 & risk <= 1))
  cal <- calibration_from_model(fit, d, "time", "status", horizon = 10, bins = 5)
  expect_true(is.data.frame(cal))
  dc <- decision_curve_from_model(fit, d, "time", "status", horizon = 10)
  expect_true(is.data.frame(dc))
})

# --- R5-12 (zip_reports) ------------------------------------------------------

test_that("zip_reports() renders a factor path column by its labels, not its codes (R5-12)", {
  reports <- data.frame(name = c("a", "b"), path = factor(c("x.txt", "b.qmd")))
  expect_error(zip_reports(reports), "these do not: x.txt", fixed = TRUE)
})

test_that("zip_reports() renders a numeric path column by its values (R5-12)", {
  reports <- data.frame(name = c("a", "b"), path = c(123, 456))
  expect_error(zip_reports(reports), "these do not: 123, 456", fixed = TRUE)
})

test_that("zip_reports() still renders a character path as before (R5-12)", {
  reports <- data.frame(name = c("a", "b"), path = c("x.txt", "b.qmd"), stringsAsFactors = FALSE)
  expect_error(zip_reports(reports), "these do not: x.txt", fixed = TRUE)
})

test_that("zip_reports() still names the row of an NA path (R5-12)", {
  reports <- data.frame(name = c("a", "b"), path = c("a.qmd", NA_character_), stringsAsFactors = FALSE)
  expect_error(zip_reports(reports), "<NA> (row 2)", fixed = TRUE)
})

test_that("zip_reports() still shows an empty path as <empty> (R5-12)", {
  reports <- data.frame(name = c("a", "b"), path = c("", "b.qmd"), stringsAsFactors = FALSE)
  expect_error(zip_reports(reports), "<empty> (row 1)", fixed = TRUE)
})

test_that("zip_reports() still renders a mixed bad-path column in full (R5-12)", {
  reports <- data.frame(
    name = c("a", "b", "c", "d"),
    path = c("", NA_character_, "x.txt", "d.qmd"),
    stringsAsFactors = FALSE
  )
  expect_error(
    zip_reports(reports),
    "<empty> (row 1), <NA> (row 2), x.txt",
    fixed = TRUE
  )
})
