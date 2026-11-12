#' Recurrent-Event PROC PHREG Parity (SAS Recurrent-Admission Baseline)
#'
#' The SAS recurrent-admission baseline: a `PROC PHREG` counterpart to the
#' package's recurrent-event Cox fits. The SAS side is the benchmark program
#' `inst/sas/benchmark_recurrent_phreg.sas` (plain `PROC PHREG`, no macro);
#' the R side here fits the same models from [recurrent_cox()], parses a
#' SAS listing back into the same shape, and compares the two.
#'
#' @section Benchmark data and model:
#' Both sides fit `survival::cgd`, the chronic granulomatous disease trial
#' (203 counting-process intervals for 128 patients, 76 recurrent
#' infections), as returned by [cgd_recurrent_data()]: `treat` recoded to
#' 1 = rIFN-g, 0 = placebo, `sex` to 1 = female, 0 = male, `age` in years.
#' The model on both sides is
#' `(tstart, tstop) * status(0) = treat sex age`, with `TIES=EFRON` in SAS
#' (matching [recurrent_cox()], which fits with Efron ties -- not the SAS
#' default Breslow, which moves the coefficients in the 4th-5th decimal)
#' and `COVS(AGGREGATE)` with `ID id` in SAS (the Lin-Wei robust sandwich
#' standard errors that [recurrent_cox()] reports). Two fits are compared:
#' `"ag"` (Andersen-Gill, total time, one baseline hazard) and
#' `"pwp_gap"` (Prentice-Williams-Peterson gap time, baseline hazard per
#' event number, event numbers pooled at `max_events = 3` -- strata 1, 2
#' and 3-or-later, holding 44, 17 and 15 events -- on gap times that
#' restart at the previous infection). No other `recurrent_cox()` model
#' has a SAS counterpart in this benchmark.
#'
#' @section Status:
#' The program was run once on SAS 9.4 (TS1M8, Windows) on 12 Nov 2026; the
#' listing is stored as `inst/sas/list/benchmark_recurrent_phreg.lst` and the
#' log as `inst/sas/logs/benchmark_recurrent_phreg.log` (no errors or
#' warnings). The R quantities agree with that listing at the per-quantity
#' tolerances of [compare_recurrent_phreg()] (Tier-1 test in
#' `tests/testthat/test-round2-0911-sas.R`), so [sas_parity_boundary()] lists
#' this as `"parity"`. The claim covers the two fits and the `COVS(AGGREGATE)`
#' / `COVM` standard errors on `survival::cgd` with Efron ties only.
#'
#' @section How to run it (on a machine with SAS 9.4):
#' Export the data from R with
#' `write_sas_data(cgd_recurrent_data(), "cgdrec.xpt")`, put `cgdrec.xpt`
#' beside `inst/sas/benchmark_recurrent_phreg.sas`, and run the program
#' with `run_sas_script(sas_macro_path("benchmark_recurrent_phreg.sas"))`
#' (or in SAS directly -- the program header gives the batch command).
#' Store the listing and log as
#' `inst/sas/list/benchmark_recurrent_phreg.lst` and
#' `inst/sas/logs/benchmark_recurrent_phreg.log`. Then, in R,
#' `parse_recurrent_phreg_lst()` the stored listing and
#' `compare_recurrent_phreg()` it against
#' `recurrent_phreg_quantities(cgd_recurrent_data())`; a Tier-1 test in
#' `tests/testthat/test-round2-0911-sas.R` does exactly that once the
#' listing exists.
#'
#' @section What the listing must contain:
#' Each `PROC PHREG` run prints two "Analysis of Maximum Likelihood
#' Estimates" tables -- one "with Model-Based Variance Estimate" (from
#' `COVM`) and one "with Sandwich Variance Estimate" (from
#' `COVS(AGGREGATE)`, with an extra StdErr Ratio column). The parser keys
#' each table to its fit by the program's title lines
#' (`"Recurrent PHREG -- Andersen-Gill ..."` / `"Recurrent PHREG -- PWP
#' gap time ..."`), and to its standard-error type by the table subtitle,
#' falling back to the number of columns when a subtitle is absent. A
#' p-value SAS prints as `<.0001` parses to `NA` and matches when the R
#' p-value is below 1e-4, as in [compare_km_lifetest()].
#'
#' @param data A data frame of counting-process intervals.
#' @param predictors Covariates: a character vector of column names, or a
#'   one-sided formula such as `~ treat + sex + age`. Default
#'   `c("treat", "sex", "age")`, the benchmark model.
#' @param id,start,stop,status Column names (strings) of the subject
#'   identifier, the interval start and stop, and the event indicator
#'   (0/1 or logical).
#' @param models Which fits to run: any subset of `"ag"` and `"pwp_gap"`.
#' @param max_events For the `"pwp_gap"` fit, event numbers above
#'   `max_events` are pooled into one stratum (default `3`, the strata
#'   of the SAS program). Passed to [recurrent_cox()].
#' @param conf_level Confidence level of the hazard ratios (default `0.95`).
#' @return [cgd_recurrent_data()]: a tibble with `id`, `tstart`, `tstop`,
#'   `status`, `treat` (1 = rIFN-g), `sex` (1 = female), `age` and `enum`.
#'   [recurrent_phreg_quantities()]: a tibble with `model`, `term`,
#'   `estimate` (log hazard ratio), `std_error` (robust sandwich),
#'   `std_error_model` (model-based, from the fit's naive covariance),
#'   `hazard_ratio`, `p_value` (robust Wald) and `p_value_model`
#'   (model-based Wald). [parse_recurrent_phreg_lst()]: a tibble with
#'   `model`, `se_type` (`"robust"` or `"model"`), `term`, `estimate`,
#'   `std_error`, `std_error_ratio` (`NA` for model-based rows),
#'   `statistic` (Wald chi-square), `p_value` and `hazard_ratio`.
#'   [compare_recurrent_phreg()]: a list of class
#'   `recurrent_phreg_comparison` with `table` (one row per compared
#'   quantity, with `r_value`, `sas_value`, `abs_diff` and `matched`) and
#'   `matched` (single `TRUE`/`FALSE`).
#' @seealso [sas_parity_boundary()], [recurrent_cox()],
#'   [compare_km_lifetest()], [sas_macro_path()], [run_sas_script()],
#'   [write_sas_data()]
#' @export
#' @examples
#' cgd <- cgd_recurrent_data()
#' nrow(cgd)
#' recurrent_phreg_quantities(cgd)
cgd_recurrent_data <- function() {
  rlang::check_installed("survival", reason = "to load the cgd data.")
  d <- as.data.frame(survival::cgd)
  d <- d[order(d$id, d$tstart), , drop = FALSE]
  recode01 <- function(x, positive) {
    if (is.factor(x) || is.character(x)) {
      return(as.integer(as.character(x) == positive))
    }
    if (is.numeric(x) && all(x %in% c(0, 1))) {
      return(as.integer(x))
    }
    stop("`", positive, "` could not be recoded to 0/1.", call. = FALSE)
  }
  out <- tibble::tibble(
    id = d$id,
    tstart = as.numeric(d$tstart),
    tstop = as.numeric(d$tstop),
    status = as.integer(d$status),
    treat = recode01(d$treat, "rIFN-g"),
    sex = recode01(d$sex, "female"),
    age = as.numeric(d$age),
    enum = as.integer(d$enum)
  )
  # The SAS program derives the event number from (tstart, tstop, status)
  # instead of trusting this column; it must agree with 1 + the number of
  # earlier events of the patient, or the two sides fit different strata.
  prior <- as.numeric(stats::ave(
    out$status, out$id,
    FUN = function(v) c(0, cumsum(v)[-length(v)])
  ))
  if (!all(out$enum == prior + 1)) {
    stop("`survival::cgd$enum` is not 1 + the number of earlier events.", call. = FALSE)
  }
  out
}

#' @rdname cgd_recurrent_data
#' @export
recurrent_phreg_quantities <- function(data,
                                       predictors = c("treat", "sex", "age"),
                                       id = "id",
                                       start = "tstart",
                                       stop = "tstop",
                                       status = "status",
                                       models = c("ag", "pwp_gap"),
                                       max_events = 3L,
                                       conf_level = 0.95) {
  models <- match.arg(models, choices = c("ag", "pwp_gap"), several.ok = TRUE)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  rows <- list()
  for (model in models) {
    fit <- recurrent_cox(
      data, predictors,
      id = id, start = start, stop = stop, status = status,
      model = model, max_events = max_events, conf_level = conf_level
    )
    naive_var <- fit$fit$naive.var
    if (is.null(naive_var)) {
      stop("The `", model, "` fit has no model-based (naive) covariance.", call. = FALSE)
    }
    beta <- stats::coef(fit$fit)
    robust_se <- sqrt(diag(stats::vcov(fit$fit)))
    model_se <- sqrt(diag(naive_var))
    rows[[length(rows) + 1L]] <- tibble::tibble(
      model = model,
      term = names(beta),
      estimate = unname(beta),
      std_error = unname(robust_se),
      std_error_model = unname(model_se),
      hazard_ratio = exp(unname(beta)),
      p_value = 2 * stats::pnorm(-abs(unname(beta / robust_se))),
      p_value_model = 2 * stats::pnorm(-abs(unname(beta / model_se)))
    )
  }
  dplyr::bind_rows(rows)
}

#' @rdname cgd_recurrent_data
#' @param lines Character vector of listing lines (from a `.lst` file, as
#'   read by [readLines()]), or a single path to one.
#' @export
parse_recurrent_phreg_lst <- function(lines) {
  if (length(lines) == 1L && file.exists(lines)) {
    lines <- readLines(lines, warn = FALSE)
  }
  if (!is.character(lines)) {
    stop("`lines` must be a character vector of listing lines, or a path to a listing file.", call. = FALSE)
  }
  # Listings from a Windows SAS session can carry non-ASCII line-drawing bytes;
  # they are never part of a number, so blank them rather than fail on them.
  lines <- iconv(lines, from = "UTF-8", to = "ASCII", sub = " ")
  lines[is.na(lines)] <- ""
  # NOTE: the test helper parse_phreg_lst() in tests/testthat/helper-sas-reference.R
  # parses a single PHREG table given its terms; this exported parser is a
  # different function (it reads the benchmark's four tables -- two fits x
  # model-based/robust -- keyed by the program's title lines), so it has a
  # different name and must not be renamed to parse_phreg_lst().
  rows <- list()
  model <- NA_character_
  in_mle <- FALSE
  se_type <- NA_character_
  n_table_rows <- 0L
  for (ln in lines) {
    tr <- trimws(ln)
    if (grepl("Recurrent PHREG -- Andersen-Gill", tr, fixed = TRUE)) {
      model <- "ag"
      # A title line also ends any table still open (titles head each page).
      in_mle <- FALSE
      se_type <- NA_character_
      n_table_rows <- 0L
      next
    }
    if (grepl("Recurrent PHREG -- PWP gap time", tr, fixed = TRUE)) {
      model <- "pwp_gap"
      in_mle <- FALSE
      se_type <- NA_character_
      n_table_rows <- 0L
      next
    }
    if (grepl("Analysis of Maximum Likelihood Estimates", tr, fixed = TRUE)) {
      in_mle <- TRUE
      se_type <- NA_character_
      n_table_rows <- 0L
      next
    }
    if (!in_mle) next
    if (grepl("with Model-Based Variance Estimate", tr, fixed = TRUE)) {
      se_type <- "model"
      next
    }
    if (grepl("with Sandwich Variance Estimate", tr, fixed = TRUE)) {
      se_type <- "robust"
      next
    }
    # A table row is one parameter name, an integer DF, and then numbers:
    #   model-based:  param DF estimate stderr chisq p hr [lower upper]
    #   sandwich:     param DF estimate stderr ratio chisq p hr [lower upper]
    # (p may be the token "<.0001"). Header lines fail the integer-DF test.
    # Any line that is not a row ends the table once its rows have started;
    # before the first row, header and blank lines are skipped, so a table
    # split across a page break still parses.
    tokens <- strsplit(tr, "\\s+")[[1]]
    tokens <- tokens[nzchar(tokens)]
    parsed <- NULL
    if (length(tokens) >= 7L && length(tokens) <= 10L &&
        grepl("^[A-Za-z_][A-Za-z0-9_]*$", tokens[1])) {
      df <- suppressWarnings(as.integer(tokens[2]))
      if (!is.na(df)) {
        inferred <- if (length(tokens) %in% c(7L, 9L)) "model" else "robust"
        if (!is.na(se_type) && se_type != inferred) {
          stop(
            "Listing line does not match its table type (", se_type, "): ", tr, ".",
            call. = FALSE
          )
        }
        use_type <- if (is.na(se_type)) inferred else se_type
        pos <- if (use_type == "robust") {
          list(est = 3L, se = 4L, ratio = 5L, chisq = 6L, p = 7L, hr = 8L)
        } else {
          list(est = 3L, se = 4L, ratio = NA_integer_, chisq = 5L, p = 6L, hr = 7L)
        }
        nums <- suppressWarnings(as.numeric(tokens[c(pos$est, pos$se, pos$chisq, pos$hr)]))
        p_tok <- tokens[pos$p]
        p_val <- if (p_tok == "<.0001") NA_real_ else suppressWarnings(as.numeric(p_tok))
        if (!anyNA(nums) && (!is.na(p_val) || p_tok == "<.0001")) {
          if (is.na(model)) {
            stop("PHREG table row found before any 'Recurrent PHREG --' title line.", call. = FALSE)
          }
          parsed <- tibble::tibble(
            model = model,
            se_type = use_type,
            term = tokens[1],
            estimate = nums[1],
            std_error = nums[2],
            std_error_ratio = if (is.na(pos$ratio)) NA_real_ else suppressWarnings(as.numeric(tokens[pos$ratio])),
            statistic = nums[3],
            p_value = p_val,
            hazard_ratio = nums[4]
          )
        }
      }
    }
    if (!is.null(parsed)) {
      rows[[length(rows) + 1L]] <- parsed
      n_table_rows <- n_table_rows + 1L
    } else if (n_table_rows > 0L) {
      in_mle <- FALSE
      se_type <- NA_character_
      n_table_rows <- 0L
    }
  }
  if (length(rows) == 0L) {
    stop("Recurrent PHREG quantities could not be parsed from the listing.", call. = FALSE)
  }
  # A benchmark program may print the same table twice (for example a PROC
  # PRINT of an ODS data set after the procedure's own listing).
  dplyr::distinct(dplyr::bind_rows(rows))
}

#' @rdname cgd_recurrent_data
#' @param r,sas Quantity tables as returned by
#'   [recurrent_phreg_quantities()] and [parse_recurrent_phreg_lst()].
#' @param atol,rtol Absolute and relative tolerances for the estimates and
#'   standard errors (SAS prints those to 5 decimals). P-values use an
#'   absolute tolerance of 1e-4 and hazard ratios an absolute 6e-4,
#'   because SAS prints p-values to 4 decimals and hazard ratios to 3
#'   (half a unit in the last place is 5e-5 and 5e-4 respectively).
#' @export
compare_recurrent_phreg <- function(r, sas, atol = 6e-6, rtol = 5e-4) {
  for (nm in c("r", "sas")) {
    tbl <- get(nm)
    if (!is.data.frame(tbl)) {
      stop("`", nm, "` must be a data frame.", call. = FALSE)
    }
  }
  need_r <- c("model", "term", "estimate", "std_error", "std_error_model",
              "hazard_ratio", "p_value", "p_value_model")
  need_sas <- c("model", "se_type", "term", "estimate", "std_error", "p_value")
  if (!all(need_r %in% names(r))) {
    stop("`r` must have columns: ", paste(need_r, collapse = ", "), ".", call. = FALSE)
  }
  if (!all(need_sas %in% names(sas))) {
    stop("`sas` must have columns: ", paste(need_sas, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(atol) || length(atol) != 1L || atol < 0 || !is.numeric(rtol) || length(rtol) != 1L || rtol < 0) {
    stop("`atol` and `rtol` must be single non-negative numbers.", call. = FALSE)
  }
  sas_key <- paste(sas$model, sas$se_type, sas$term)
  find_sas <- function(model, term, se_type) {
    hit <- match(paste(model, se_type, term), sas_key)
    if (is.na(hit)) {
      stop(
        "SAS listing has no ", se_type, " row for model '", model, "', term '", term, "'.",
        call. = FALSE
      )
    }
    sas[hit, , drop = FALSE]
  }
  quantities <- list(
    list(quantity = "estimate", r_col = "estimate", sas_se = "robust", sas_col = "estimate"),
    list(quantity = "std_error_robust", r_col = "std_error", sas_se = "robust", sas_col = "std_error"),
    list(quantity = "std_error_model", r_col = "std_error_model", sas_se = "model", sas_col = "std_error"),
    list(quantity = "hazard_ratio", r_col = "hazard_ratio", sas_se = "robust", sas_col = "hazard_ratio"),
    list(quantity = "p_robust", r_col = "p_value", sas_se = "robust", sas_col = "p_value"),
    list(quantity = "p_model", r_col = "p_value_model", sas_se = "model", sas_col = "p_value")
  )
  rows <- list()
  for (i in seq_len(nrow(r))) {
    for (q in quantities) {
      srow <- find_sas(r$model[i], r$term[i], q$sas_se)
      r_value <- r[[q$r_col]][i]
      sas_value <- srow[[q$sas_col]]
      abs_diff <- abs(r_value - sas_value)
      # A p-value SAS prints as <.0001 parses to NA: it matches when R's
      # value is below 1e-4, and is otherwise unassessable rather than a failure.
      is_p <- grepl("^p_", q$quantity)
      matched <- if (is_p && is.na(sas_value)) {
        !is.na(r_value) && r_value < 1e-4
      } else if (q$quantity == "hazard_ratio") {
        # SAS prints hazard ratios to 3 decimals: half a unit in the last
        # place is 5e-4, so an absolute 6e-4 covers the rounding.
        !is.na(abs_diff) && abs_diff <= 6e-4
      } else if (is_p) {
        # SAS prints p-values to 4 decimals: half a unit there is 5e-5.
        !is.na(abs_diff) && abs_diff <= 1e-4
      } else {
        !is.na(abs_diff) && (abs_diff <= atol || abs_diff <= rtol * abs(sas_value))
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = r$model[i], term = r$term[i], quantity = q$quantity,
        r_value = r_value, sas_value = sas_value,
        abs_diff = abs_diff, matched = matched
      )
    }
  }
  table <- dplyr::bind_rows(rows)
  max_abs_diff <- if (all(is.na(table$abs_diff))) NA_real_ else max(table$abs_diff, na.rm = TRUE)
  out <- list(table = table, matched = all(table$matched), max_abs_diff = max_abs_diff)
  class(out) <- c("recurrent_phreg_comparison", "list")
  out
}

#' @rdname cgd_recurrent_data
#' @param x A `recurrent_phreg_comparison` returned by
#'   [compare_recurrent_phreg()].
#' @param ... Unused; present for consistency with [print()].
#' @export
print.recurrent_phreg_comparison <- function(x, ...) {
  cat("<recurrent_phreg_comparison> matched: ", x$matched,
      "; max |diff|: ", format(x$max_abs_diff, digits = 4), "\n", sep = "")
  print(x$table, n = nrow(x$table))
  invisible(x)
}
