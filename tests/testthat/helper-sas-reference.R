# Helpers for the SAS parity tests (test-sas_parity_reference.R, test-sas_parity_live.R).
#
# SAS results are kept in "long" form: one row per number, with columns
# quantity, term, value. reference/*.csv holds what SAS printed; parse_*_lst()
# reads the same numbers out of a SAS listing (.lst); phreg_quantities() and
# brier_quantities() produce them from R. expect_matches_sas() compares two such tables.

# SAS prints 4-5 decimals (half a unit in the last place is 5e-6; 6e-6 leaves room
# for floating point) and 3 for hazard ratios, hence the relative slack. R and SAS
# also stop their Newton iterations by different criteria (SAS: GCONV=1E-8), which
# moves estimates in the fifth decimal.
sas_atol <- 6e-6
sas_rtol <- 5e-4

# The full-lung program reads survival::lung itself (a test in
# test-sas_parity_reference.R checks that its datalines are that data), so R needs
# no SAS here: same recode and `delete` as the program, with the dots of the column
# names turned into underscores.
lung_full_data <- function() {
  lung <- as.data.frame(survival::lung)
  names(lung) <- gsub(".", "_", names(lung), fixed = TRUE)
  lung$event <- as.numeric(lung$status == 2)
  lung[!is.na(lung$ph_karno), ]
}

lung_model <- function(lung) {
  cbe_cox_multi(lung, survival::Surv(time, event) ~ age + sex + ph_karno, ties = "breslow")$model
}

# Numbers SAS produced once. `#` lines at the top of each file give the SAS
# release, the run date, and the listing the numbers were copied from.
sas_reference <- function(file) {
  utils::read.csv(
    testthat::test_path("reference", file),
    comment.char = "#",
    colClasses = c(quantity = "character", term = "character", value = "numeric")
  )
}

# The `datalines;` block of a SAS program as a data frame (`csv = TRUE` for
# `infile datalines dsd`). Reading the data out of the program keeps R and SAS
# on the very same numbers.
sas_datalines <- function(path, col_names, csv = FALSE) {
  lines <- trimws(readLines(path, warn = FALSE))
  start <- grep("^datalines;$", lines)[1] + 1L
  end <- which(lines == ";")
  end <- end[end >= start][1] - 1L
  utils::read.table(
    text = lines[start:end], sep = if (csv) "," else "", header = FALSE,
    na.strings = ".", col.names = col_names
  )
}

# Numbers after `pattern` on the first listing line that matches it. SAS prints
# "<.0001" for a tiny p-value; that comes back as NA.
sas_numbers <- function(lines, pattern) {
  hit <- grep(pattern, lines)[1]
  if (is.na(hit)) {
    return(numeric())
  }
  rest <- trimws(sub(pattern, "", lines[hit]))
  suppressWarnings(as.numeric(strsplit(rest, "\\s+")[[1]]))
}

long_rows <- function(quantity, term, value) {
  data.frame(quantity = quantity, term = as.character(term), value = value, stringsAsFactors = FALSE)
}

# PROC PHREG output of a listing: one row per covariate for the estimate,
# standard error, Wald chi-square, p-value and hazard ratio, plus the fit
# statistics and the three global tests (term "model").
parse_phreg_lst <- function(lines, terms) {
  per_term <- lapply(terms, function(term) {
    v <- sas_numbers(lines, paste0("^\\s*", term, "\\s+1\\s+"))
    long_rows(c("estimate", "std_error", "chisq", "p", "hazard_ratio"), term, v[1:5])
  })
  fit <- function(label) sas_numbers(lines, paste0("^\\s*", label, "\\s+"))
  model <- long_rows(
    c(
      "neg2loglik_null", "neg2loglik_model", "aic", "sbc",
      "lr_chisq", "score_chisq", "wald_chisq"
    ),
    "model",
    c(
      fit("-2 LOG L")[1:2], fit("AIC")[2], fit("SBC")[2],
      fit("Likelihood Ratio")[1], fit("Score")[1], fit("Wald")[1]
    )
  )
  do.call(rbind, c(per_term, list(model)))
}

# The same quantities from a fitted coxph. SAS's SBC uses the number of events.
# summary.coxph() rounds the Wald test to 2 decimals, so it is read from the model.
phreg_quantities <- function(model) {
  b <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model)))
  ll <- model$loglik
  k <- length(b)
  fit <- summary(model)
  per_term <- lapply(names(b), function(term) {
    long_rows(
      c("estimate", "std_error", "chisq", "p", "hazard_ratio"), term,
      c(b[[term]], se[[term]], (b[[term]] / se[[term]])^2, 2 * stats::pnorm(-abs(b[[term]] / se[[term]])), exp(b[[term]]))
    )
  })
  model_rows <- long_rows(
    c(
      "neg2loglik_null", "neg2loglik_model", "aic", "sbc",
      "lr_chisq", "score_chisq", "wald_chisq"
    ),
    "model",
    c(
      -2 * ll[1], -2 * ll[2], -2 * ll[2] + 2 * k, -2 * ll[2] + k * log(model$nevent),
      fit$logtest[["test"]], fit$sctest[["test"]], model$wald.test
    )
  )
  do.call(rbind, c(per_term, list(model_rows)))
}

# %cbe_brier_score output of a listing: one row per evaluation time (a listing
# with a row per subject would give duplicate terms, which expect_matches_sas()
# rejects). The integrated Brier score has term "ibs".
parse_brier_lst <- function(lines) {
  as_numbers <- function(x) suppressWarnings(as.numeric(strsplit(trimws(x), "\\s+")[[1]]))
  ibs_at <- grep("Integrated Brier Score \\(IBS\\)", lines)[1]
  rows <- lapply(lines[seq_len(ibs_at - 1L)], as_numbers)
  rows <- do.call(rbind, Filter(function(v) length(v) == 5L && !anyNA(v), rows))
  # columns: eval_time, n_eval, n_events_by_t, n_at_risk, brier_score
  ibs <- Filter(function(v) length(v) == 3L && !anyNA(v), lapply(lines[ibs_at:length(lines)], as_numbers))[[1]]
  do.call(rbind, list(
    long_rows("n_events_by_t", rows[, 1], rows[, 3]),
    long_rows("n_at_risk", rows[, 1], rows[, 4]),
    long_rows("brier_score", rows[, 1], rows[, 5]),
    long_rows("ibs", "ibs", ibs[3])
  ))
}

# The same quantities from R, by the pipeline of the SAS-comparison vignette:
# predicted survival of a coxph `model` at the evaluation times, Graf censoring
# weights, yardstick's Brier score and integrated Brier score. `data` is what
# the model was fit on, with time and event (1 = event) columns.
brier_quantities <- function(model, data, eval_times) {
  surv <- summary(survival::survfit(model, newdata = data), times = eval_times)$surv
  preds <- lapply(seq_len(nrow(data)), function(i) {
    tibble::tibble(.eval_time = eval_times, .pred_survival = surv[, i])
  })
  truth <- survival::Surv(data$time, data$event)
  scored <- add_graf_weights(
    tibble::tibble(.truth = truth, .pred = preds),
    censoring = censoring_km(truth)
  )
  brier <- yardstick::brier_survival(scored, truth = .truth, .pred)
  ibs <- yardstick::brier_survival_integrated(scored, truth = .truth, .pred)
  do.call(rbind, list(
    long_rows("n_events_by_t", eval_times, vapply(eval_times, function(t) sum(data$time <= t & data$event == 1), 1)),
    long_rows("n_at_risk", eval_times, vapply(eval_times, function(t) sum(data$time > t), 1)),
    long_rows("brier_score", eval_times, brier$.estimate[match(eval_times, brier$.eval_time)]),
    long_rows("ibs", "ibs", ibs$.estimate)
  ))
}

# `r` and `sas` are long tables (quantity, term, value). Every SAS value must be
# matched by R within `atol` + `rtol` * |SAS|; a SAS value that is NA (printed
# "<.0001") is skipped. Both tables must hold the same quantity/term rows, each
# once, so a row deleted from the reference (or a value R stops reporting) or a
# row listed twice fails.
expect_matches_sas <- function(r, sas, atol = 0, rtol = 0) {
  keys <- function(x) paste(x$quantity, x$term, sep = " / ")
  unmatched <- c(
    setdiff(keys(sas), keys(r)), setdiff(keys(r), keys(sas)),
    unique(c(keys(r)[duplicated(keys(r))], keys(sas)[duplicated(keys(sas))]))
  )
  m <- merge(sas, r, by = c("quantity", "term"), suffixes = c("_sas", "_r"), all.x = TRUE, sort = FALSE)
  m <- m[!is.na(m$value_sas), ]
  off <- is.na(m$value_r) | abs(m$value_r - m$value_sas) > atol + rtol * abs(m$value_sas)
  testthat::expect(
    nrow(m) > 0 && !any(off) && !length(unmatched),
    paste0(
      if (nrow(m) == 0) "No SAS values to compare with\n",
      if (length(unmatched)) paste0("Missing from one of R and SAS, or listed twice: ", paste(unmatched, collapse = ", "), "\n"),
      if (any(off)) {
        paste0(
          "R differs from SAS in ", sum(off), " of ", nrow(m), " values:\n",
          paste0(
            sprintf("  %s [%s]: R %s, SAS %s", m$quantity[off], m$term[off], format(m$value_r[off]), format(m$value_sas[off])),
            collapse = "\n"
          )
        )
      }
    )
  )
  invisible(m)
}

# Runs a SAS program (any path) with its log and listing in a temporary folder
# (never beside the program) and returns the listing's lines, with the log's
# lines as attribute "log". SAS exit status 1 (warnings) is accepted; 2 or more
# is an error in run_sas_script().
run_sas_file <- function(path) {
  out <- withr::local_tempdir()
  status <- suppressWarnings(run_sas_script(path, log_dir = out, list_dir = out))
  stem <- sub("\\.sas$", "", basename(path))
  log <- readLines(file.path(out, paste0(stem, ".log")), warn = FALSE)
  testthat::expect_false(any(grepl("^ERROR", log)), info = "SAS log has ERROR lines")
  testthat::expect_lte(status, 1L)
  structure(readLines(file.path(out, paste0(stem, ".lst")), warn = FALSE), log = log)
}

# The same for a program bundled in inst/sas/.
run_bundled_sas <- function(script) {
  run_sas_file(cbe_sas_macro_path(script))
}

# Writes a SAS program that runs %cbe_brier_score (PHREG predictions, Breslow
# ties, trunc = 0.05) on `lung_days` (columns time in whole days, event, age, sex,
# ph_karno) with follow-up in years: the program divides the days by 365.25, and
# R does the same division on the same doubles. `eval_times` (in years) is pasted
# into the macro call as text, so decimals such as 0.5 reach the macro exactly as
# a user types them. Returns the path of the program, in `dir`.
write_brier_driver <- function(lung_days, eval_times, dir) {
  stopifnot(all(lung_days$time == round(lung_days$time)))
  rows <- with(lung_days, paste(time, event, age, sex, ph_karno))
  program <- c(
    sprintf('%%include "%s";', cbe_sas_macro_path("cbe_brier_score.sas")),
    "",
    "data work.lung_years;",
    "    input days event age sex ph_karno;",
    "    time = days / 365.25;",
    "datalines;",
    rows,
    ";",
    "run;",
    "",
    "%cbe_brier_score(",
    "    data       = work.lung_years,",
    "    time       = time,",
    "    status     = event,",
    "    pred_vars  = age sex ph_karno,",
    sprintf("    eval_times = %s,", paste(eval_times, collapse = " ")),
    "    ties       = BRESLOW,",
    "    trunc      = 0.05,",
    "    out_brier  = work.lung_brier,",
    "    out_ibs    = work.lung_ibs,",
    "    out_calib  = work.lung_calib",
    ");"
  )
  path <- file.path(dir, "brier_lung_years.sas")
  writeLines(program, path)
  path
}

# Writes a SAS program that runs %cbe_counting_process on `wide` (columns id,
# time, dead and the measurements P1..Pk, NA for a missing one) and prints every
# row of its output to the listing as
#   CBE_ROW <id> <T1> <T2> <Status> <Covariate>
# with "." for a missing value; counting_process_rows() reads them back.
# `obs_times` is pasted into the macro call as text, so decimals such as 0.5
# reach the macro exactly as a user types them. Returns the path of the program,
# in `dir` (two programs of one test need two `stem`s: SAS names its log after it).
write_counting_driver <- function(wide, obs_times, dir, stem) {
  sas_value <- function(x) ifelse(is.na(x), ".", as.character(x))
  rows <- do.call(paste, lapply(wide, sas_value))
  program <- c(
    sprintf('%%include "%s";', cbe_sas_macro_path("cbe_counting_process.sas")),
    "",
    "data work.wide;",
    sprintf("    input %s;", paste(names(wide), collapse = " ")),
    "datalines;",
    rows,
    ";",
    "run;",
    "",
    "%cbe_counting_process(",
    "    data_wide = work.wide,",
    "    id        = id,",
    "    time      = time,",
    "    dead      = dead,",
    sprintf("    obs_times = %s,", paste(obs_times, collapse = " ")),
    "    out_data  = work.long",
    ");",
    "",
    "data _null_;",
    "    set work.long;",
    "    file print;",
    '    put "CBE_ROW " id best12. +1 T1 best12. +1 T2 best12. +1 Status best12. +1 Covariate best12.;',
    "run;"
  )
  path <- file.path(dir, paste0(stem, ".sas"))
  writeLines(program, path)
  path
}

# The CBE_ROW lines of a listing written by write_counting_driver(), as a data
# frame with columns id, T1, T2, Status, Covariate (NA for SAS's ".").
counting_process_rows <- function(lst) {
  rows <- sub("^\\s*CBE_ROW\\s+", "", grep("^\\s*CBE_ROW\\s", lst, value = TRUE))
  utils::read.table(
    text = rows, header = FALSE, na.strings = ".",
    col.names = c("id", "T1", "T2", "Status", "Covariate"), colClasses = "numeric"
  )
}

# PROC PRINCOMP (correlation matrix) listing: the eigenvalue table, one row per
# component for each of the eigenvalue, the difference to the next eigenvalue (the
# last component has none), the proportion and the cumulative proportion. The
# term is the component number.
parse_princomp_lst <- function(lines) {
  start <- grep("Eigenvalues of the Correlation Matrix", lines)[1]
  end <- grep("^\\s*Eigenvectors\\s*$", lines)[1] - 1L
  rows <- strsplit(trimws(lines[(start + 1L):end]), "\\s+")
  rows <- Filter(function(v) length(v) >= 4L && !anyNA(suppressWarnings(as.numeric(v))), rows)
  rows <- lapply(rows, as.numeric)
  pick <- function(i) vapply(rows, function(v) if (length(v) == 5L) v[i] else NA_real_, numeric(1))
  comp <- vapply(rows, function(v) v[1], numeric(1))
  last <- function(v) v[length(v)]
  out <- rbind(
    long_rows("eigenvalue", comp, vapply(rows, function(v) v[2], numeric(1))),
    long_rows("difference", comp, pick(3)),
    long_rows("proportion", comp, vapply(rows, function(v) v[length(v) - 1L], numeric(1))),
    long_rows("cumulative", comp, vapply(rows, last, numeric(1)))
  )
  out[!is.na(out$value), ]
}

# The same table from proc_pca(): the eigenvalue and the difference (SAS prints 8
# decimals), the proportion and the cumulative proportion as fractions.
princomp_quantities <- function(res) {
  k <- seq_len(nrow(res))
  out <- rbind(
    long_rows("eigenvalue", k, res$eigenvalue),
    long_rows("difference", k, res$difference),
    long_rows("proportion", k, res$proportion),
    long_rows("cumulative", k, res$cum_variance_pct / 100)
  )
  out[!is.na(out$value), ]
}

# SAS prints the eigenvalues and their differences to 8 decimals (half a unit in
# the last place is 5e-9; 6e-9 leaves room for floating point) and the
# proportions to 4 (5e-5; 5.1e-5).
sas_princomp_fine <- c("eigenvalue", "difference")
sas_princomp_atol <- c(fine = 6e-9, coarse = 5.1e-5)

expect_matches_sas_princomp <- function(r, sas) {
  is_fine <- function(x) x$quantity %in% sas_princomp_fine
  expect_matches_sas(r[is_fine(r), ], sas[is_fine(sas), ], atol = sas_princomp_atol[["fine"]])
  expect_matches_sas(r[!is_fine(r), ], sas[!is_fine(sas), ], atol = sas_princomp_atol[["coarse"]])
}

# ---------------------------------------------------------------------------
# PROC COMPARE (METHOD=ABSOLUTE) against cbe_compare_df()
#
# benchmark_proc_compare.sas compares nine pairs of small data sets typed into the
# program. Each pair is a "scenario": its listing starts with a line titled
# "Scenario <name>:", and tests/testthat/reference/sas_proc_compare.csv holds the
# numbers that listing shows (columns scenario, statistic, value). In the tables
# below the statistic is the `quantity` and the scenario the `term`, so that
# expect_matches_sas() can compare them like the other benchmarks.
#
# Statistics (see compare_statistic_map for where each comes from):
#   one per scenario   the counts of the Variables, Observation and Values
#                      Comparison Summaries, "Maximum Difference", and sysinfo
#   one per variable   ndif[<var>], maxdif[<var>], missdif[<var>]
#   one per cell       unequal[<var>@<key>] (1) and diff[<var>@<key>] for the cells
#                      PROC COMPARE lists as unequal; <key> is the value of the ID
#                      variable, or the observation number without one
# ---------------------------------------------------------------------------

# The data of the nine scenarios, as the R fixtures: a test checks that they are the
# numbers typed into the SAS program. s6_same compares s1_base with a copy of itself.
compare_fixtures <- function() {
  df <- function(...) data.frame(..., stringsAsFactors = FALSE)
  list(
    s1_base = df(
      id = as.numeric(1:10),
      same_num = seq(10, 100, by = 10),
      within_tol = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
      over_tol = as.numeric(5:14),
      miss_num = c(1.1, 2.2, 3.3, NA, 5.5, 6.6, 7.7, 8.8, NA, 10.1),
      chr_diff = c("abc", "abc", "ghi", "jkl", "mno", "pqr", "stu", "vwx", "yza", "bcd"),
      chr_blank = c("x", "y", "z", "w", "", "v", "u", "y", "", "t"),
      chr_len = c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa"),
      conf = as.numeric(1:10),
      only_base = seq(100, 1000, by = 100)
    ),
    # typed unsorted: PROC COMPARE needs sorted data, cbe_compare_df() does not
    s1_comp = df(
      id = c(11, 5, 1, 3, 2, 4, 6, 8, 9),
      same_num = c(110, 50, 10, 30, 20, 40, 60, 80, 90),
      within_tol = c(11.5, 5.5, 1.50000005, 3.5, 2.5, 4.5, 6.49999997, 8.5, 9.5),
      over_tol = c(15, 11.5, 5, 7.001, 6.00000008, 8, 10, 12.000001, 13),
      miss_num = c(11.1, 5.5, 1.1, 3.3, 2.2, 4.4, NA, 8.8, NA),
      chr_diff = c("efg", "mno", "abc", "ghi", "abd", "jkl", "Pqr", "vwx", "yza"),
      chr_blank = c("s", "x", "x", "z", "y", "w", "v", "", ""),
      chr_len = c("lambda", "epsilon", "alpha", "gamma", "beta", "delta", "zeta", "theta", "iota"),
      conf = c("11", "5", "1", "3", "2.0", "4", "6", "8", "9"),
      only_comp = c("k", "e", "a", "c", "b", "d", "f", "h", "i")
    ),
    s2_base = df(
      a = as.numeric(1:7),
      b = seq(10, 70, by = 10),
      c = c(100, 200, NA, 400, 500, 600, 700),
      s = c("p", "q", "r", "s", "t", "u", "v")
    ),
    s2_comp = df(
      a = as.numeric(1:5),
      b = c(10, 21.5, 30, 40, 50),
      c = c(100, 200, 300, 400, 500.0000002),
      s = c("p", "q", "x", "s", "t")
    ),
    s3_base = df(id = c(1, 1, 2, 3, 3, 3), v = c(10, 11, 20, 30, 31, 32)),
    s3_comp = df(id = c(1, 1, 1, 2, 3, 3), v = c(10, 11, 12, 20, 30, 99)),
    s4_base = df(id = as.numeric(1:7), x = rep(1, 7)),
    s4_comp = df(
      id = as.numeric(1:7),
      x = c(1.25, 1.2500001, 1.2499999, 0.75, 1.5, 0.7499999, 1.250000000000002)
    ),
    s5_base = df(id = as.numeric(1:8), x = c(1, 1, 100, 100, 0.001, 0.001, 1, 1)),
    s5_comp = df(
      id = as.numeric(1:8),
      x = c(1.00000009, 1.00000011, 100.00000009, 100.00000011, 0.00100009, 0.00100011, 0.99999991, 0.99999989)
    ),
    s7_base = df(id = as.numeric(1:4), x = c(1, 100, 0.1, 5)),
    s7_comp = df(id = as.numeric(1:4), x = c(1.00000005, 100.00000005, 0.10000005, 5.00000005)),
    s8_base = df(id = as.numeric(1:4), s = c("abc", "def", "ghi", "jkl"), n = as.numeric(1:4)),
    s8_comp = df(id = as.numeric(1:4), s = c("abc", "def  ", "GHI", "jkl"), n = as.numeric(1:4)),
    s9_base = df(id = as.numeric(1:3), x = c(1, 1, 0.5)),
    s9_comp = df(id = as.numeric(1:3), x = c(1.1, 1.09999, 0.6))
  )
}

# What each scenario asks of cbe_compare_df(): `by` is PROC COMPARE's ID statement
# (NULL: observations are compared by position) and `tolerance` its CRITERION.
compare_scenarios <- list(
  s1_id      = list(base = "s1_base", compare = "s1_comp", by = "id", tolerance = 1e-7),
  s2_rows    = list(base = "s2_base", compare = "s2_comp", by = NULL, tolerance = 1e-7),
  s3_dup     = list(base = "s3_base", compare = "s3_comp", by = "id", tolerance = 1e-7),
  s4_edge    = list(base = "s4_base", compare = "s4_comp", by = "id", tolerance = 0.25),
  s5_decimal = list(base = "s5_base", compare = "s5_comp", by = "id", tolerance = 1e-7),
  s6_same    = list(base = "s1_base", compare = "s1_base", by = "id", tolerance = 1e-7),
  s7_within  = list(base = "s7_base", compare = "s7_comp", by = "id", tolerance = 1e-7),
  s8_text    = list(base = "s8_base", compare = "s8_comp", by = "id", tolerance = 1e-7),
  s9_trap    = list(base = "s9_base", compare = "s9_comp", by = "id", tolerance = 0.1)
)

# One data set typed into a SAS program (`data work.<name>; ... input ...; datalines;`)
# as a data frame: column names and types come from its input statement (a variable
# followed by `$` is character), "." is a missing number and an empty field a
# blank. Nothing is trimmed, so a trailing blank in a value stays.
sas_dataset <- function(path, name) {
  lines <- readLines(path, warn = FALSE)
  header <- grep(paste0("^data work\\.", name, ";\\s*$"), lines)[1]
  stopifnot(!is.na(header))
  input_at <- header + grep("^\\s*input\\s", lines[(header + 1L):length(lines)])[1]
  tokens <- strsplit(trimws(sub(";\\s*$", "", sub("^\\s*input\\s+", "", lines[input_at]))), "\\s+")[[1]]
  vars <- tokens[tokens != "$"]
  is_char <- tokens[match(vars, tokens) + 1L] %in% "$"
  start <- header + grep("^datalines;\\s*$", lines[(header + 1L):length(lines)])[1] + 1L
  end <- start - 1L + which(lines[start:length(lines)] == ";")[1] - 1L
  utils::read.table(
    text = lines[start:end], sep = ",", header = FALSE, col.names = vars,
    colClasses = ifelse(is_char, "character", "numeric"), na.strings = ".",
    quote = "", comment.char = "", strip.white = FALSE, fill = TRUE, stringsAsFactors = FALSE
  )
}

# The PROC COMPARE calls of the program, one per scenario: the data sets, the
# CRITERION and the ID variable ("" for none). A scenario's title line comes first.
sas_compare_calls <- function(path) {
  lines <- readLines(path, warn = FALSE)
  titles <- grep("^title \"Scenario \\w+:", lines)
  do.call(rbind, lapply(titles, function(i) {
    call_at <- i + grep("^proc compare\\s", lines[(i + 1L):length(lines)])[1]
    call <- lines[call_at]
    arg <- function(name) sub(paste0("^.*\\s", name, "=work\\.(\\w+).*$"), "\\1", call)
    data.frame(
      scenario = sub("^title \"Scenario (\\w+):.*$", "\\1", lines[i]),
      base = arg("base"), compare = arg("compare"),
      criterion = as.numeric(sub("^.*\\scriterion=(\\S+)\\s.*$", "\\1", call)),
      id = if (grepl("^\\s*id\\s", lines[call_at + 1L])) trimws(sub(";.*$", "", sub("^\\s*id\\s+", "", lines[call_at + 1L]))) else "",
      stringsAsFactors = FALSE
    )
  }))
}

# cbe_compare_df() on a scenario's fixtures, once at the scenario's tolerance and
# once at 0 (PROC COMPARE's "not EXACTLY equal" count), and whether it warned
# about duplicate keys (it pairs such rows in order, as PROC COMPARE does).
compare_run <- function(name, fixtures = compare_fixtures(), tolerance = NULL) {
  spec <- compare_scenarios[[name]]
  if (!is.null(tolerance)) {
    spec$tolerance <- tolerance
  }
  duplicate_warning <- FALSE
  run <- function(tolerance) {
    withCallingHandlers(
      cbe_compare_df(
        fixtures[[spec$base]], fixtures[[spec$compare]],
        by = spec$by, tolerance = tolerance, base_name = "base", compare_name = "compare"
      ),
      cbe_compare_df_duplicate_keys = function(w) {
        duplicate_warning <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
  }
  list(name = name, spec = spec, cmp = run(spec$tolerance), exact = run(0), duplicate_warning = duplicate_warning)
}

# Where each statistic of the reference comes from in SAS's listing (BASE and COMPARE
# stand for the two data set names) and in cbe_compare_df()'s result, one row each;
# NA: no counterpart. PROC COMPARE does not compare a variable whose type differs
# between the data sets; cbe_compare_df() does (as text), so the R side of the
# "compared" statistics leaves such variables out (types_match = FALSE). The
# last five are per variable (ndif, maxdif, missdif) and per cell (unequal, diff).
compare_statistic_map <- as.data.frame(do.call(rbind, list(
  c("vars_common", "Number of Variables in Common", "length(variables$common)"),
  c("vars_base_only", "Number of Variables in BASE but not in COMPARE", "length(variables$base_only)"),
  c("vars_compare_only", "Number of Variables in COMPARE but not in BASE", "length(variables$compare_only)"),
  c("vars_conflicting_type", "Number of Variables with Conflicting Types", "sum(!summary$types_match)"),
  c("vars_differing_attr", "Number of Variables with Differing Attributes", NA),
  c("id_vars", "Number of ID Variables", "length(meta$by)"),
  c("obs_common", "Number of Observations in Common", "observations$n_matched"),
  c("obs_base_only", "Number of Observations in BASE but not in COMPARE", "observations$unmatched_base"),
  c("obs_compare_only", "Number of Observations in COMPARE but not in BASE", "observations$unmatched_compare"),
  c("dup_obs_base", "Number of Duplicate Observations found in BASE", NA),
  c("dup_obs_compare", "Number of Duplicate Observations found in COMPARE", NA),
  c("obs_read_base", "Total Number of Observations Read from BASE", "meta$n_base"),
  c("obs_read_compare", "Total Number of Observations Read from COMPARE", "meta$n_compare"),
  c("obs_some_unequal", "Number of Observations with Some Compared Variables Unequal",
    "distinct (row_base, row_compare) in diffs"),
  c("obs_all_equal", "Number of Observations with All Compared Variables Equal",
    "observations$n_matched minus obs_some_unequal"),
  c("vars_all_equal", "Number of Variables Compared with All Observations Equal", "sum(summary$n_diff == 0)"),
  c("vars_some_unequal", "Number of Variables Compared with Some Observations Unequal", "sum(summary$n_diff > 0)"),
  c("vars_missing_diff", "Number of Variables with Missing Value Differences",
    "variables with a diffs row that is NA or empty on one side only"),
  c("values_unequal", "Total Number of Values which Compare Unequal", "sum(summary$n_diff)"),
  c("values_not_exact", "Total Number of Values not EXACTLY Equal", "the same sum with tolerance = 0"),
  c("max_diff", "Maximum Difference", "max(summary$max_diff)"),
  c("sysinfo", "&SYSINFO after the PROC COMPARE", NA),
  c("ndif", "Ndif of Variables with Unequal Values", "summary$n_diff"),
  c("maxdif", "MaxDif of Variables with Unequal / All Equal Values", "summary$max_diff (numeric variables)"),
  c("missdif", "MissDif of Variables with Unequal Values", "diffs rows that are NA or empty on one side only"),
  c("unequal", "a row of Value Comparison Results for Variables", "a row of diffs"),
  c("diff", "Diff. there (COMPARE minus BASE)", "-diffs$diff (diffs$diff is base minus compare)")
)), stringsAsFactors = FALSE)
names(compare_statistic_map) <- c("statistic", "sas", "cbe_compare_df")

# In the reference but without a number from cbe_compare_df(): the attributes it does
# not compare (length, here), the duplicate-ID counts (it warns, class
# cbe_compare_df_duplicate_keys, instead) and SYSINFO, a bit mask that
# compare_sysinfo_r() translates.
compare_statistics_unmapped <- compare_statistic_map$statistic[is.na(compare_statistic_map$cbe_compare_df)]

# &SYSINFO bits cbe_compare_df() has a counterpart for: observations only in BASE
# (64) or COMPARE (128), variables only in BASE (1024) or COMPARE (2048), unequal
# values (4096) and conflicting types (8192). Bits 1 to 32 (data set label and type,
# informat, format, length, label) are attributes cbe_compare_df() does not look at.
compare_sysinfo_mask <- 64L + 128L + 1024L + 2048L + 4096L + 8192L

compare_sysinfo_r <- function(run) {
  cmp <- run$cmp
  s <- cmp$summary
  compared <- s[s$types_match, , drop = FALSE]
  64L * (cmp$observations$unmatched_base > 0) +
    128L * (cmp$observations$unmatched_compare > 0) +
    1024L * (length(cmp$variables$base_only) > 0) +
    2048L * (length(cmp$variables$compare_only) > 0) +
    4096L * (sum(compared$n_diff) > 0) +
    8192L * any(!s$types_match)
}

# Statistics of compare_statistic_map from a compare_run() (see there for the
# meaning of each). A value is missing if it is NA or an empty string (SAS's blank).
compare_quantities <- function(run) {
  cmp <- run$cmp
  by <- cmp$meta$by
  s <- cmp$summary
  compared <- s[s$types_match, , drop = FALSE]
  d <- cmp$diffs
  if (nrow(d) > 0) {
    d <- d[d$variable %in% compared$variable, , drop = FALSE]
  }
  # diffs has no columns at all when nothing differs (guarded: `$` on a missing
  # tibble column warns)
  any_diff <- nrow(d) > 0
  is_missing <- function(x) is.na(x) | x %in% ""
  d_variable <- if (any_diff) d$variable else character()
  d_missing <- if (any_diff) xor(is_missing(d$base_value), is_missing(d$compare_value)) else logical()
  missdif <- vapply(compared$variable, function(v) sum(d_missing[d_variable == v]), numeric(1))
  key <- if (!any_diff) {
    character()
  } else if (is.null(by)) {
    as.character(d$row_base)
  } else {
    do.call(paste, c(lapply(by, function(b) as.character(d[[b]])), sep = "/"))
  }
  cell <- paste0(d_variable, "@", key)
  both_present <- if (any_diff) !is.na(d$diff) else logical()

  n_some <- if (any_diff) nrow(unique(d[, c("row_base", "row_compare")])) else 0L
  scalar <- c(
    vars_common = length(cmp$variables$common),
    vars_base_only = length(cmp$variables$base_only),
    vars_compare_only = length(cmp$variables$compare_only),
    vars_conflicting_type = sum(!s$types_match),
    id_vars = length(by),
    obs_common = cmp$observations$n_matched,
    obs_base_only = cmp$observations$unmatched_base,
    obs_compare_only = cmp$observations$unmatched_compare,
    obs_read_base = cmp$meta$n_base,
    obs_read_compare = cmp$meta$n_compare,
    obs_some_unequal = n_some,
    obs_all_equal = cmp$observations$n_matched - n_some,
    vars_all_equal = sum(compared$n_diff == 0),
    vars_some_unequal = sum(compared$n_diff > 0),
    vars_missing_diff = sum(missdif > 0),
    values_unequal = sum(compared$n_diff),
    values_not_exact = sum(run$exact$summary$n_diff[run$exact$summary$types_match]),
    max_diff = if (all(is.na(compared$max_diff))) 0 else max(compared$max_diff, na.rm = TRUE)
  )
  numeric_vars <- compared[!is.na(compared$max_diff), , drop = FALSE]
  do.call(rbind, list(
    long_rows(names(scalar), run$name, unname(scalar)),
    compare_rows("ndif", compared$variable, run$name, compared$n_diff),
    compare_rows("maxdif", numeric_vars$variable, run$name, numeric_vars$max_diff),
    compare_rows("missdif", compared$variable, run$name, missdif),
    compare_rows("unequal", cell, run$name, rep(1, length(cell))),
    compare_rows("diff", cell[both_present], run$name, if (any_diff) -d$diff[both_present] else numeric())
  ))
}

# Rows "<family>[<label>]" of the scenario (none if there is no label).
compare_rows <- function(family, label, scenario, value) {
  long_rows(sprintf("%s[%s]", family, label), rep(scenario, length(label)), value)
}

# The numbers of one scenario's part of a PROC COMPARE listing (`lines`), as the
# tables of the other benchmarks. SAS leaves a count out when it is zero, and the
# Values Comparison Summary out when all values are exactly equal; those are
# read as 0 (a test checks this against the committed listing), "not EXACTLY
# Equal" as the unequal count when it is left out, and the number of variables
# with all values equal from the table that lists them.
parse_compare_scenario <- function(lines, scenario) {
  names_at <- grep("^\\s*Comparison of \\S+ with \\S+\\s*$", lines)[1]
  ds <- strsplit(trimws(lines[names_at]), "\\s+")[[1]]
  base_ds <- ds[3]
  comp_ds <- ds[5]
  # the lines of the summaries: every statistic of the map but SYSINFO and the
  # per-variable and per-cell ones
  scalar_rows <- !compare_statistic_map$statistic %in% c("sysinfo", "ndif", "maxdif", "missdif", "unequal", "diff")
  labels <- stats::setNames(compare_statistic_map$sas[scalar_rows], compare_statistic_map$statistic[scalar_rows])
  labels <- gsub("COMPARE", comp_ds, gsub("BASE", base_ds, labels, fixed = TRUE), fixed = TRUE)
  number_after <- function(label) {
    hit <- grep(label, lines, fixed = TRUE)[1]
    if (is.na(hit)) {
      return(NA_real_)
    }
    as.numeric(sub("^.*:\\s*(\\S+?)\\.\\s*$", "\\1", lines[hit], perl = TRUE))
  }
  scalar <- vapply(labels, number_after, numeric(1))

  tables <- parse_compare_variable_tables(lines)
  if (is.na(scalar[["vars_all_equal"]])) {
    scalar[["vars_all_equal"]] <- sum(tables$ndif == 0)
  }
  zero_when_left_out <- c(
    "vars_base_only", "vars_compare_only", "vars_conflicting_type", "vars_differing_attr", "id_vars",
    "obs_base_only", "obs_compare_only", "dup_obs_base", "dup_obs_compare",
    "vars_some_unequal", "vars_missing_diff", "values_unequal", "max_diff"
  )
  left_out <- zero_when_left_out[is.na(scalar[zero_when_left_out])]
  scalar[left_out] <- 0
  if (is.na(scalar[["values_not_exact"]])) {
    scalar[["values_not_exact"]] <- scalar[["values_unequal"]]
  }
  stopifnot(!anyNA(scalar))

  sysinfo_at <- grep(paste0("^SYSINFO ", scenario, " "), lines)[1]
  scalar <- c(scalar, sysinfo = as.numeric(sub("^SYSINFO \\S+ (\\d+)\\s*$", "\\1", lines[sysinfo_at])))

  cells <- parse_compare_cells(lines)
  has_diff <- !is.na(cells$diff)
  has_maxdif <- !is.na(tables$maxdif)
  cell <- paste0(cells$variable, "@", cells$key)
  do.call(rbind, list(
    long_rows(names(scalar), scenario, unname(scalar)),
    compare_rows("ndif", tables$variable, scenario, tables$ndif),
    compare_rows("maxdif", tables$variable[has_maxdif], scenario, tables$maxdif[has_maxdif]),
    compare_rows("missdif", tables$variable, scenario, tables$missdif),
    compare_rows("unequal", cell, scenario, rep(1, length(cell))),
    compare_rows("diff", cell[has_diff], scenario, cells$diff[has_diff])
  ))
}

# "Variables with All Equal Values", "Variables with Unequal Values" and "All
# Variables Compared have Unequal Values": one row per compared variable with the
# columns Ndif (0 in the first), MaxDif (blank for a character variable: NA) and
# MissDif (0 where the table has no such column). The columns are right-aligned
# under their headings, so a value is assigned to the heading it ends under.
parse_compare_variable_tables <- function(lines) {
  headers <- which(grepl("^\\s*Variable\\s+Type\\s", lines) & grepl("MaxDif", lines))
  rows <- lapply(headers, function(h) {
    pos <- gregexpr("\\S+", lines[h])[[1]]
    ends <- pos + attr(pos, "match.length") - 1L
    cols <- substring(lines[h], pos, ends)
    out <- list()
    r <- h + 2L
    while (r <= length(lines) && nzchar(trimws(lines[r]))) {
      tp <- gregexpr("\\S+", lines[r])[[1]]
      te <- tp + attr(tp, "match.length") - 1L
      tokens <- substring(lines[r], tp, te)
      value <- stats::setNames(rep(NA_character_, length(cols)), cols)
      value[1] <- tokens[1]
      for (k in seq_along(tokens)[-(1:2)]) {
        j <- 2L + which.min(abs(ends[-(1:2)] - te[k]))
        stopifnot(abs(ends[j] - te[k]) <= 1L)
        value[j] <- tokens[k]
      }
      num <- function(col, default) if (col %in% cols) suppressWarnings(as.numeric(value[[col]])) else default
      out[[length(out) + 1L]] <- data.frame(
        variable = value[[1]],
        ndif = num("Ndif", 0), maxdif = num("MaxDif", NA_real_), missdif = num("MissDif", 0),
        stringsAsFactors = FALSE
      )
      r <- r + 1L
    }
    do.call(rbind, out)
  })
  out <- do.call(rbind, rows)
  out$missdif[is.na(out$missdif)] <- 0
  out
}

# "Value Comparison Results for Variables": one row per cell listed as unequal,
# with the variable, the key (value of the ID variable, or the observation number)
# and, for a numeric variable with both values present, the Diff. column.
parse_compare_cells <- function(lines) {
  out <- list()
  var <- NA_character_
  numeric_block <- FALSE
  expect_variable <- FALSE
  in_rows <- FALSE
  for (l in lines) {
    has_bars <- grepl("||", l, fixed = TRUE)
    if (grepl("^\\s*\\|\\|\\s+Base", l)) {
      numeric_block <- !grepl("Base Value", l, fixed = TRUE)
      expect_variable <- TRUE
      in_rows <- FALSE
    } else if (expect_variable && has_bars) {
      var <- strsplit(trimws(sub("^.*\\|\\|", "", l)), "\\s+")[[1]][1]
      expect_variable <- FALSE
    } else if (grepl("^\\s*_+\\s+\\|\\|\\s+_", l)) {
      in_rows <- TRUE
    } else if (in_rows && has_bars && nzchar(trimws(sub("\\|\\|.*$", "", l)))) {
      key <- strsplit(trimws(sub("\\|\\|.*$", "", l)), "\\s+")[[1]][1]
      shown <- strsplit(trimws(sub("^.*\\|\\|", "", l)), "\\s+")[[1]]
      diff <- if (numeric_block) suppressWarnings(as.numeric(shown[3])) else NA_real_
      out[[length(out) + 1L]] <- data.frame(variable = var, key = key, diff = diff, stringsAsFactors = FALSE)
    } else if (!has_bars && grepl("^\\s*_+\\s*$", l)) {
      in_rows <- FALSE
    }
  }
  if (length(out) == 0L) {
    return(data.frame(variable = character(), key = character(), diff = numeric()))
  }
  do.call(rbind, out)
}

# The scenarios of a listing written by benchmark_proc_compare.sas.
parse_compare_lst <- function(lines) {
  is_title <- grepl("^\\s*Scenario \\w+:", lines)
  at <- cummax(ifelse(is_title, seq_along(lines), 0L))
  scenario <- ifelse(at > 0L, sub("^\\s*Scenario (\\w+):.*$", "\\1", lines[pmax(at, 1L)]), NA_character_)
  do.call(rbind, lapply(unique(stats::na.omit(scenario)), function(s) {
    parse_compare_scenario(lines[!is.na(scenario) & scenario == s], s)
  }))
}

# The numbers SAS printed, as copied into reference/sas_proc_compare.csv.
sas_compare_reference <- function(file = "sas_proc_compare.csv") {
  ref <- utils::read.csv(
    testthat::test_path("reference", file),
    comment.char = "#",
    colClasses = c(scenario = "character", statistic = "character", value = "numeric")
  )
  long_rows(ref$statistic, ref$scenario, ref$value)
}

# The family of a statistic: its name without the [variable] or [variable@key].
compare_family <- function(statistic) sub("\\[.*$", "", statistic)

# The differences (maxdif, diff, max_diff) are what SAS printed in at most 8
# characters (2.500, 1.1E-7, 0.001000): a relative slack of 5e-4 covers the rounding
# and still rejects a different number. It cannot hide a count that is off by one
# (the counts here are far below 2000), and a SAS 0 must be matched exactly.
expect_matches_sas_compare <- function(r, sas) expect_matches_sas(r, sas, rtol = 5e-4)

# The part of a table that has a counterpart in cbe_compare_df().
compare_mapped <- function(x) x[!compare_family(x$quantity) %in% compare_statistics_unmapped, ]
