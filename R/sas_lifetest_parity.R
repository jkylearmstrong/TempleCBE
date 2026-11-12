#' Kaplan-Meier / PROC LIFETEST Parity (First Increment Beyond the SAS Boundary)
#'
#' The first increment of broader SAS parity (B5): a `PROC LIFETEST`
#' counterpart to the package's Kaplan-Meier tooling. The SAS side is the
#' `%cbe_lifetest` macro in `inst/sas/cbe_lifetest.sas` (with the benchmark
#' program `inst/sas/benchmark_lifetest_km.sas`); the R side here computes the
#' same quantities from [survival::survfit()] / [survival::survdiff()],
#' parses a SAS listing back into the same long shape, and compares the two.
#'
#' @section Quantities compared:
#' Product-limit survival estimates (one row per event time per stratum:
#' `quantity = "km_surv"`, `term` = stratum, `time` = event time,
#' `value` = survival probability), the median survival time
#' (`quantity = "median"`, `time = NA` — the first event time at which the
#' product-limit estimate is at most 0.5, which is SAS's definition; not
#' [stats::quantile()] on the `survfit` object, which interpolates when
#' survival hits exactly 0.5 at an event time), and, when a grouping variable is
#' given, the log-rank test (`"logrank_chisq"`, `"logrank_df"`,
#' `"logrank_p"`, `term = "model"`). Standard errors and confidence limits
#' are computed by `survfit()` but are not part of the comparison: SAS prints
#' them with the Greenwood formula on a log-log scale by default, while
#' `survfit()` defaults differ, so they are a follow-up, not a first
#' increment.
#'
#' @section Status:
#' Run once on SAS 9.4 (TS1M8, Temple University licence, 11 November 2026).
#' The benchmark program `inst/sas/benchmark_lifetest_km.sas` was run and its
#' listing stored at `inst/sas/list/benchmark_lifetest_km.lst` (log at
#' `inst/sas/logs/benchmark_lifetest_km.log`). [parse_lifetest_lst()] parses
#' the stored listing and [compare_km_lifetest()] agrees with
#' [km_lifetest_quantities()] on the benchmark data (13 rows: 8 KM survival
#' estimates, both medians, the log-rank test) at the tolerances the Tier-1
#' test uses (`atol = 1e-4`, `rtol = 1e-3`; SAS prints 4 decimals), asserted
#' in `tests/testthat/test-gaps-0811-w2-data.R`; [sas_parity_boundary()]
#' lists PROC LIFETEST as `"parity"` on that evidence. No reference CSV is
#' kept: the test parses the stored listing directly, so no
#' `tests/testthat/reference/sas_lifetest_km.csv` exists or is needed. Note
#' the stored log still echoes the pre-run "DRAFT / has not been run" comment
#' text from the program as it stood at run time; the program header has
#' since been updated, and the log is kept as run evidence, not re-written.
#'
#' @section Follow-up PROCs (not in this increment):
#' `PROC LOGISTIC` (counterpart [glm_single()]), `PROC MIXED` / `GLIMMIX`
#' ([lmm_single()] / [piecewise_lmm()]), `PROC FREQ`
#' ([tbl_contingency_2x2()]) and `PROC MEANS` ([summary_table()]) remain
#' `"not_started"` in [sas_parity_boundary()]; each needs its own SAS macro,
#' benchmark program typed on the same numbers, and reference listing, in
#' the style of this increment.
#'
#' @param data A data frame.
#' @param time Name of the follow-up-time column (numeric, non-negative).
#' @param status Name of the event-indicator column (0/1, or `TRUE`/`FALSE`).
#' @param group Optional name of a grouping column (strata for the log-rank
#'   test). `NULL` (default) fits a single curve.
#' @param conf_level Confidence level for `survfit()` (stored, not compared).
#' @return [km_lifetest_quantities()]: a tibble with `quantity`, `term`,
#'   `time` and `value`. [parse_lifetest_lst()]: the same shape, parsed from
#'   listing lines. [compare_km_lifetest()]: a list of class
#'   `km_lifetest_comparison` with `table` (one row per compared quantity,
#'   with `r_value`, `sas_value`, `abs_diff` and `matched`) and `matched`
#'   (single `TRUE`/`FALSE`).
#' @seealso [sas_parity_boundary()], [km_single()], [sas_macro_path()],
#'   [run_sas_script()]
#' @param x The object to print (the result of the function).
#' @param ... Not used.
#' @export
#' @examples
#' df <- data.frame(
#'   time = c(5, 6, 6, 8, 10, 12, 3, 4, 7, 9, 9, 11),
#'   status = c(1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0),
#'   group = rep(c("A", "B"), each = 6)
#' )
#' km_lifetest_quantities(df, "time", "status", group = "group")
km_lifetest_quantities <- function(data, time, status, group = NULL, conf_level = 0.95) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` must have at least one row.", call. = FALSE)
  }
  for (col in c(time, status, group)) {
    if (!is.null(col) && (!is.character(col) || length(col) != 1L || !col %in% names(data))) {
      stop("`", col, "` must name one column of `data`.", call. = FALSE)
    }
  }
  t <- data[[time]]
  if (!is.numeric(t) || anyNA(t) || any(!is.finite(t)) || any(t < 0)) {
    stop("`", time, "` must be finite, non-negative numbers with no missing values.", call. = FALSE)
  }
  s <- data[[status]]
  if (is.logical(s)) s <- as.integer(s)
  if (!is.numeric(s) || anyNA(s) || !all(s %in% c(0, 1))) {
    stop("`", status, "` must be 0/1 (or TRUE/FALSE) with no missing values.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }
  strata <- if (is.null(group)) rep("all", nrow(data)) else as.character(data[[group]])
  if (anyNA(strata)) {
    stop("`", group, "` must have no missing values.", call. = FALSE)
  }

  rows <- list()
  for (st in unique(strata)) {
    idx <- strata == st
    fit <- survival::survfit(
      survival::Surv(t[idx], s[idx]) ~ 1, conf.type = "log-log", conf.int = conf_level
    )
    # SAS's median is the first event time at which the product-limit
    # estimate is <= 0.5 (NA when survival never falls that far), except that
    # when the estimate is exactly 0.5 there it is the midpoint of the flat
    # step, up to the next event time. Checked against a PROC LIFETEST run on
    # SAS 9.4: stratum B of the benchmark (survival exactly 0.5 from t = 7 to
    # t = 9) prints a median of 8, which is also what stats::quantile(fit)
    # returns; this is spelled out here so the rule does not depend on it.
    ev <- fit$n.event > 0
    ev_time <- fit$time[ev]
    ev_surv <- fit$surv[ev]
    med <- NA_real_
    if (any(ev_surv <= 0.5 + 1e-12)) {
      j <- which(ev_surv <= 0.5 + 1e-12)[1L]
      med <- ev_time[j]
      if (abs(ev_surv[j] - 0.5) < 1e-12 && j < length(ev_time)) {
        med <- (ev_time[j] + ev_time[j + 1L]) / 2
      }
    }
    rows[[length(rows) + 1L]] <- tibble::tibble(
      quantity = "km_surv", term = st, time = fit$time[ev], value = fit$surv[ev]
    )
    rows[[length(rows) + 1L]] <- tibble::tibble(
      quantity = "median", term = st, time = NA_real_, value = med
    )
  }
  out <- dplyr::bind_rows(rows)
  if (!is.null(group) && length(unique(strata)) >= 2L) {
    lr <- survival::survdiff(survival::Surv(t, s) ~ strata)
    df_lr <- length(unique(strata)) - 1L
    chisq <- unname(lr$chisq)
    out <- dplyr::bind_rows(out, tibble::tibble(
      quantity = c("logrank_chisq", "logrank_df", "logrank_p"),
      term = "model",
      time = NA_real_,
      value = c(chisq, df_lr, stats::pchisq(chisq, df_lr, lower.tail = FALSE))
    ))
  }
  out
}

#' @rdname km_lifetest_quantities
#' @param lines Character vector of listing lines (from a `.lst` file, as
#'   read by [readLines()]), or a single path to one.
#' @export
parse_lifetest_lst <- function(lines) {
  if (length(lines) == 1L && file.exists(lines)) {
    lines <- readLines(lines, warn = FALSE)
  }
  if (!is.character(lines)) {
    stop("`lines` must be a character vector of listing lines, or a path to a listing file.", call. = FALSE)
  }
  # The benchmark program prints the product-limit table under the heading
  # "Product-Limit Survival Estimates" with columns
  #   Time  Survival  Failure  StdErr  NumberFailed  NumberLeft
  # (the layout SAS 9.4 PROC LIFETEST prints for METHOD=PL), the median in
  # the "Quartiles" table as "50% (Median)", and the log-rank test in the
  # "Test of Equality over Strata" table on the "Log-Rank" row. Strata are
  # introduced by a line "Stratum <label>" above their table.
  rows <- list()
  stratum <- "all"
  in_km <- FALSE
  in_quart <- FALSE
  in_test <- FALSE
  for (ln in lines) {
    tr <- trimws(ln)
    if (grepl("^Stratum\\s+", tr)) {
      # SAS 9.4 prints "Stratum 1: group = A"; the stratum is its value, here
      # "A", as on the R side. A bare "Stratum A" is also accepted. The
      # "Stratum  group  Total ..." header of the censoring summary is not a
      # stratum label and is ignored.
      lab <- trimws(sub("^Stratum\\s+", "", tr))
      if (grepl("^[0-9]+\\s*:", lab)) {
        stratum <- trimws(sub("^[0-9]+\\s*:\\s*[^=]+=\\s*", "", lab))
      } else if (!grepl("\\s", lab)) {
        stratum <- lab
      }
      next
    }
    if (grepl("Product-Limit Survival Estimates", tr, fixed = TRUE)) {
      in_km <- TRUE; in_quart <- FALSE; in_test <- FALSE; next
    }
    if ((grepl("Quartiles", tr, fixed = TRUE) || grepl("Quartile Estimates", tr, fixed = TRUE)) &&
        !grepl("Product-Limit", tr, fixed = TRUE)) {
      in_km <- FALSE; in_quart <- TRUE; in_test <- FALSE; next
    }
    if (grepl("Test of Equality over Strata", tr, fixed = TRUE)) {
      in_km <- FALSE; in_quart <- FALSE; in_test <- TRUE; next
    }
    if (in_km) {
      if (tr == "" || grepl("^[A-Za-z]", tr) || grepl("^---", tr) || grepl("^\\s*Time\\b", ln)) next
      nums <- suppressWarnings(as.numeric(strsplit(tr, "\\s+")[[1]]))
      # the time-0 row (survival 1, nobody failed yet) is not an event time;
      # a censored time prints "." for the survival and is skipped as NA
      if (length(nums) >= 2L && !anyNA(nums[1:2]) && nums[1] > 0) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          quantity = "km_surv", term = stratum, time = nums[1], value = nums[2]
        )
      }
      next
    }
    if (in_quart) {
      # e.g. "50% (Median)   8.0000" or "50% (Median)  8.0000  .  ."
      # SAS 9.4 prints "50   8.0000   LOGLOG   5.0000   ."; a "." point
      # estimate means the median is not reached (NA).
      m <- regmatches(tr, regexec("^50(?:%\\s*\\(Median\\))?\\s+([0-9.eE+-]+)", tr, perl = TRUE))[[1]]
      if (length(m) == 2L) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          quantity = "median", term = stratum, time = NA_real_,
          value = suppressWarnings(as.numeric(m[2]))
        )
        in_quart <- FALSE
      }
      next
    }
    if (in_test) {
      # e.g. "Log-Rank   1.2345   1   0.2665"
      m <- regmatches(tr, regexec("^Log-Rank\\s+([0-9.eE+-]+)\\s+([0-9]+)\\s+([0-9.eE+-]+|<\\.0001)", tr))[[1]]
      if (length(m) == 4L) {
        chisq <- as.numeric(m[2])
        df <- as.numeric(m[3])
        p <- if (m[4] == "<.0001") NA_real_ else as.numeric(m[4])
        rows[[length(rows) + 1L]] <- tibble::tibble(
          quantity = c("logrank_chisq", "logrank_df", "logrank_p"),
          term = "model", time = NA_real_, value = c(chisq, df, p)
        )
        in_test <- FALSE
      }
      next
    }
  }
  if (length(rows) == 0L) {
    stop("PROC LIFETEST quantities could not be parsed from the listing.", call. = FALSE)
  }
  # a benchmark program may print the same table twice (for example a
  # PROC PRINT of an ODS data set after the procedure's own listing)
  dplyr::distinct(dplyr::bind_rows(rows))
}

#' @rdname km_lifetest_quantities
#' @param r,sas Long-form quantity tables as returned by
#'   [km_lifetest_quantities()] and [parse_lifetest_lst()].
#' @param atol,rtol Absolute and relative tolerances, as in the other SAS
#'   parity checks (`helper-sas-reference.R`: SAS prints 4-5 decimals).
#'   The defaults are set to the listing's own precision: SAS prints 4
#'   decimals, so rounding alone can move a small quantity by up to 5e-05,
#'   and `atol = 6e-5` accepts that while still failing a genuinely
#'   different value.
#' @export
compare_km_lifetest <- function(r, sas, atol = 6e-5, rtol = 5e-4) {
  for (nm in c("r", "sas")) {
    tbl <- get(nm)
    if (!is.data.frame(tbl) || !all(c("quantity", "term", "time", "value") %in% names(tbl))) {
      stop("`", nm, "` must be a data frame with `quantity`, `term`, `time` and `value`.", call. = FALSE)
    }
  }
  .check_compare_tolerances(atol, rtol)
  # Two empty tables would compare as a vacuous match.
  if (nrow(r) == 0L || nrow(sas) == 0L) {
    stop("`r` and `sas` must each have at least one row to compare.", call. = FALSE)
  }
  key_r <- paste(r$quantity, r$term, ifelse(is.na(r$time), "NA", format(r$time, digits = 12)))
  key_s <- paste(sas$quantity, sas$term, ifelse(is.na(sas$time), "NA", format(sas$time, digits = 12)))
  m <- match(key_r, key_s)
  if (anyNA(m)) {
    missing <- unique(paste(r$quantity[is.na(m)], r$term[is.na(m)], r$time[is.na(m)]))
    stop("SAS listing has no row for R quantities: ", paste(missing, collapse = "; "), ".", call. = FALSE)
  }
  sas_value <- sas$value[m]
  abs_diff <- abs(r$value - sas_value)
  # A p-value SAS prints as <.0001 parses to NA: it matches when R's value
  # is below 1e-4, and is otherwise unassessable rather than a failure.
  # Likewise a quantity that is NA on both sides (for example a median
  # neither side reaches) is agreement, not a failure.
  both_na <- is.na(r$value) & is.na(sas_value)
  matched <- ifelse(
    is.na(sas_value) & r$quantity == "logrank_p",
    !is.na(r$value) & r$value < 1e-4,
    both_na | (!is.na(abs_diff) & (abs_diff <= atol | abs_diff <= rtol * abs(sas_value)))
  )
  table <- tibble::tibble(
    quantity = r$quantity, term = r$term, time = r$time,
    r_value = r$value, sas_value = sas_value, abs_diff = abs_diff, matched = matched
  )
  out <- list(
    table = table, matched = all(matched),
    max_abs_diff = if (all(is.na(abs_diff))) NA_real_ else max(abs_diff, na.rm = TRUE)
  )
  class(out) <- c("km_lifetest_comparison", "list")
  out
}

# Shared by compare_km_lifetest() and compare_recurrent_phreg(): the
# tolerances must be single finite, non-negative numbers (NA or Inf would
# error cryptically or match everything).
.check_compare_tolerances <- function(atol, rtol) {
  ok <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v) && v >= 0
  if (!ok(atol) || !ok(rtol)) {
    stop("`atol` and `rtol` must be single finite, non-negative numbers.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @rdname km_lifetest_quantities
#' @export
print.km_lifetest_comparison <- function(x, ...) {
  cat("<km_lifetest_comparison> matched: ", x$matched,
      "; max |diff|: ", format(x$max_abs_diff, digits = 4), "\n", sep = "")
  print(x$table, n = nrow(x$table))
  invisible(x)
}
