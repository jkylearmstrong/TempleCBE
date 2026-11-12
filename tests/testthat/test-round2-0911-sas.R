# SAS recurrent-admission baseline (round 2, draft only).
# The SAS half is a draft -- no SAS licence is available here, so the
# program has never been run and no stored listing exists yet. The tests
# below check the R fits, the export the user will run, and the listing
# parser + comparison against a SYNTHETIC listing: R values formatted the
# way SAS 9.4 prints them, built from recurrent_phreg_quantities() at test
# time. That listing is NOT SAS output and the tests claim no agreement
# with SAS. The Tier-1 test at the end skips cleanly until the real stored
# listing exists, then exercises it (LIFETEST pattern).

# A synthetic PROC PHREG listing for the benchmark: the R quantities,
# formatted as SAS 9.4 prints them (5 decimals for estimates and standard
# errors, 4 for chi-square, 4 for p-values with "<.0001" for tiny ones,
# 3 for hazard ratios and StdErr ratios). Includes the global-null table
# and a PROC PRINT of the ODS ParameterEstimates data set, which the
# parser must ignore, and both "with Model-Based / Sandwich Variance
# Estimate" tables for each fit.
synthetic_recurrent_phreg_lst <- function() {
  q <- recurrent_phreg_quantities(cgd_recurrent_data())
  row_for <- function(model, term, se_type) {
    r <- q[q$model == model & q$term == term, , drop = FALSE]
    if (se_type == "model") {
      se <- r$std_error_model
      chisq <- (r$estimate / se)^2
      p <- r$p_value_model
      sprintf(
        "         %-8s 1 %10.5f %10.5f %10.4f %8s %8.3f",
        term, r$estimate, se, chisq,
        if (p < 1e-4) "<.0001" else sprintf("%.4f", p),
        r$hazard_ratio
      )
    } else {
      se <- r$std_error
      chisq <- (r$estimate / se)^2
      p <- r$p_value
      sprintf(
        "         %-8s 1 %10.5f %10.5f %8.3f %10.4f %8s %8.3f",
        term, r$estimate, se, se / r$std_error_model, chisq,
        if (p < 1e-4) "<.0001" else sprintf("%.4f", p),
        r$hazard_ratio
      )
    }
  }
  fit_block <- function(title, model) {
    c(
      "", "                                       The PHREG Procedure", "",
      paste0("         ", title), "",
      "                               Testing Global Null Hypothesis: BETA=0", "",
      "                       Test                 Chi-Square       DF     Pr > ChiSq", "",
      "                       Likelihood Ratio        23.5243        2         <.0001",
      "                       Wald (COVM)             29.1437        2         <.0001",
      "                       Wald (COVS)             53.9374        2         <.0001", "",
      "                              Analysis of Maximum Likelihood Estimates",
      "                              with Model-Based Variance Estimate", "",
      "                             Parameter      Standard                                  Hazard",
      "         Parameter    DF      Estimate         Error    Chi-Square    Pr > ChiSq       Ratio", "",
      row_for(model, "treat", "model"),
      row_for(model, "sex", "model"),
      row_for(model, "age", "model"), "",
      "                              Analysis of Maximum Likelihood Estimates",
      "                              with Sandwich Variance Estimate", "",
      "                             Parameter      Standard   StdErr                               Hazard",
      "         Parameter    DF      Estimate         Error    Ratio    Chi-Square    Pr > ChiSq       Ratio", "",
      row_for(model, "treat", "robust"),
      row_for(model, "sex", "robust"),
      row_for(model, "age", "robust"), "",
      "         ODS ParameterEstimates data set: printed for the record", "",
      "         Parameter    DF     Estimate    StdErr    ChiSq    ProbChiSq    HazardRatio",
      # A PROC PRINT row looks like a model-based table row; the parser
      # must ignore it (it is not inside an MLE table).
      sprintf(
        "         %-8s 1 %10.5f %10.5f %10.4f %8.4f %8.3f",
        "treat", -1.11911, 0.31018, 13.0175, 0.0003, 0.327
      ), ""
    )
  }
  c(
    "                                           The SAS System", "",
    fit_block("Recurrent PHREG -- Andersen-Gill (total time): COVS(AGGREGATE) COVM", "ag"),
    fit_block("Recurrent PHREG -- PWP gap time: COVS(AGGREGATE) COVM", "pwp_gap")
  )
}

test_that("cgd_recurrent_data() is the cgd benchmark data, recoded 0/1", {
  d <- cgd_recurrent_data()
  expect_equal(
    names(d),
    c("id", "tstart", "tstop", "status", "treat", "sex", "age", "enum")
  )
  expect_equal(nrow(d), 203)
  expect_equal(length(unique(d$id)), 128)
  expect_equal(sum(d$status == 1), 76)
  expect_equal(sum(d$status == 0), 127)
  expect_true(all(d$treat %in% c(0, 1)))
  expect_true(all(d$sex %in% c(0, 1)))
  expect_equal(sum(d$treat == 1), 83)
  expect_equal(sum(d$sex == 1), 35)
  expect_false(anyNA(d))
  # enum is 1 + the number of earlier events of the patient (the SAS
  # program derives the same number from (tstart, tstop, status)).
  d <- d[order(d$id, d$tstart), , drop = FALSE]
  prior <- as.numeric(stats::ave(
    d$status, d$id, FUN = function(v) c(0, cumsum(v)[-length(v)])
  ))
  expect_equal(d$enum, prior + 1)
})

test_that("benchmark_recurrent_phreg.sas is a PHREG program with a recorded run and ASCII formchar", {
  path <- sas_macro_path("benchmark_recurrent_phreg.sas")
  expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("RUN ONCE on SAS 9.4", txt, fixed = TRUE))
  expect_false(grepl("NEVER BEEN RUN", txt, fixed = TRUE))
  expect_true(grepl("formchar=", txt, fixed = TRUE))
  expect_true(grepl("proc phreg", txt, fixed = TRUE))
  expect_true(grepl("covs(aggregate)", txt, fixed = TRUE))
  expect_true(grepl("covm", txt, fixed = TRUE))
  expect_true(grepl("ties=efron", txt, fixed = TRUE))
  expect_true(grepl("id id;", txt, fixed = TRUE))
  expect_true(grepl("strata enum3", txt, fixed = TRUE))
  expect_true(grepl("gap_tstart", txt, fixed = TRUE))
  expect_true(grepl("gap_tstop", txt, fixed = TRUE))
  expect_true(grepl("cgdrec.xpt", txt, fixed = TRUE))
  expect_true(grepl("write_sas_data", txt, fixed = TRUE))
  expect_true(grepl("benchmark_recurrent_phreg.lst", txt, fixed = TRUE))
  expect_true(grepl("benchmark_recurrent_phreg.log", txt, fixed = TRUE))
  # The program needs no %cbe_* macro: %cbe_cox_phreg has no COVS/STRATA
  # option, so it is never called (it is named once, in the header, as
  # the reason why the program calls PROC PHREG directly).
  expect_false(grepl("%cbe_cox_phreg\\(", txt))
})

test_that("cgd_recurrent_data() round-trips through write_sas_data() (.xpt)", {
  skip_if_not_installed("haven")
  d <- cgd_recurrent_data()
  p <- file.path(tempdir(), "cgdrec.xpt")
  write_sas_data(d, p)
  back <- read_sas_data(p)
  expect_equal(nrow(back), 203)
  expect_equal(names(back), names(d))
  expect_equal(as.numeric(back$tstart), d$tstart)
  expect_equal(as.numeric(back$tstop), d$tstop)
  expect_equal(as.numeric(back$status), d$status)
  expect_equal(as.numeric(back$treat), d$treat)
  expect_equal(as.numeric(back$sex), d$sex)
  expect_equal(as.numeric(back$age), d$age)
  expect_equal(as.numeric(back$enum), d$enum)
})

test_that("recurrent_phreg_quantities() matches direct coxph fits on cgd", {
  d <- cgd_recurrent_data()
  q <- recurrent_phreg_quantities(d)
  expect_equal(sort(unique(q$model)), c("ag", "pwp_gap"))
  expect_equal(sort(unique(q$term)), c("age", "sex", "treat"))
  expect_equal(nrow(q), 6)
  expect_true(all(c(
    "model", "term", "estimate", "std_error", "std_error_model",
    "hazard_ratio", "p_value", "p_value_model"
  ) %in% names(q)))
  expect_true(all(is.finite(q$estimate)) && all(q$std_error > 0))
  # Andersen-Gill against a direct clustered coxph fit (Efron ties).
  ag <- survival::coxph(
    survival::Surv(tstart, tstop, status) ~ treat + sex + age + cluster(id),
    data = d, ties = "efron"
  )
  qa <- q[q$model == "ag", , drop = FALSE]
  expect_equal(qa$estimate, unname(stats::coef(ag)))
  expect_equal(qa$std_error, unname(sqrt(diag(stats::vcov(ag)))))
  expect_equal(qa$std_error_model, unname(sqrt(diag(ag$naive.var))))
  # Clustering matters for the AG fit: the robust SE is larger than the
  # model-based one (treatment: 0.310 vs 0.261).
  expect_gt(qa$std_error[qa$term == "treat"], qa$std_error_model[qa$term == "treat"])
  # PWP gap time against an independently built gap-time fit: gap times
  # and the capped event number reconstructed row by row here.
  prev_event <- numeric(nrow(d))
  k <- integer(nrow(d))
  for (subj in unique(d$id)) {
    idx <- which(d$id == subj)
    last <- 0
    n_ev <- 0L
    for (j in idx) {
      k[j] <- n_ev + 1L
      prev_event[j] <- last
      if (d$status[j] == 1) {
        last <- d$tstop[j]
        n_ev <- n_ev + 1L
      }
    }
  }
  g <- d
  g$gap0 <- d$tstart - prev_event
  g$gap1 <- d$tstop - prev_event
  g$kc <- pmin(k, 3)
  gp <- survival::coxph(
    survival::Surv(gap0, gap1, status) ~ treat + sex + age + strata(kc) + cluster(id),
    data = g, ties = "efron"
  )
  qg <- q[q$model == "pwp_gap", , drop = FALSE]
  expect_equal(qg$estimate, unname(stats::coef(gp)))
  expect_equal(qg$std_error, unname(sqrt(diag(stats::vcov(gp)))))
  expect_equal(qg$std_error_model, unname(sqrt(diag(gp$naive.var))))
  # The gap fit's strata hold 44 / 17 / 15 events (1, 2, 3+).
  fit <- recurrent_cox(d, ~ treat + sex + age, model = "pwp_gap", max_events = 3)
  expect_equal(as.numeric(fit$strata_events), c(44, 17, 15))
})

test_that("recurrent_phreg_quantities() validates its inputs", {
  d <- cgd_recurrent_data()
  expect_error(
    recurrent_phreg_quantities(d, models = "pwp_total"),
    "should be one of"
  )
  expect_error(recurrent_phreg_quantities(d, id = "nope"), "missing column")
  expect_error(recurrent_phreg_quantities(list(a = 1)), "must be a data frame")
})

test_that("parse_recurrent_phreg_lst() reads a synthetic listing and compare_recurrent_phreg() agrees", {
  sas <- parse_recurrent_phreg_lst(synthetic_recurrent_phreg_lst())
  expect_equal(nrow(sas), 12) # 2 fits x 2 SE types x 3 terms
  expect_setequal(sas$model, c("ag", "pwp_gap"))
  expect_setequal(sas$se_type, c("model", "robust"))
  expect_setequal(sas$term, c("treat", "sex", "age"))
  # The sandwich table's StdErr Ratio is parsed; model rows have none.
  expect_true(all(!is.na(sas$std_error_ratio[sas$se_type == "robust"])))
  expect_true(all(is.na(sas$std_error_ratio[sas$se_type == "model"])))
  # The AG treat model p-value was printed as <.0001: it parses to NA.
  expect_true(is.na(sas$p_value[
    sas$model == "ag" & sas$se_type == "model" & sas$term == "treat"
  ]))
  r <- recurrent_phreg_quantities(cgd_recurrent_data())
  cmp <- compare_recurrent_phreg(r, sas)
  expect_true(cmp$matched)
  expect_equal(nrow(cmp$table), 36) # 6 model/term rows x 6 quantities
  expect_true(all(cmp$table$matched))
  # A perturbed SAS value must fail the comparison, not pass silently.
  sas_bad <- sas
  sas_bad$std_error[
    sas_bad$model == "ag" & sas_bad$se_type == "robust" & sas_bad$term == "treat"
  ] <- 0.20
  expect_false(compare_recurrent_phreg(r, sas_bad)$matched)
  # A listing missing one table is an error, not a silent partial match.
  sas_short <- sas[!(sas$model == "pwp_gap" & sas$se_type == "model"), , drop = FALSE]
  expect_error(compare_recurrent_phreg(r, sas_short), "no model row")
  expect_error(parse_recurrent_phreg_lst("nothing to parse here"), "could not be parsed")
})

test_that("parse_recurrent_phreg_lst() infers the SE type from the columns when the subtitle is absent", {
  lst <- synthetic_recurrent_phreg_lst()
  lst <- lst[!grepl("with (Model-Based|Sandwich) Variance Estimate", lst)]
  sas <- parse_recurrent_phreg_lst(lst)
  expect_equal(nrow(sas), 12)
  r <- recurrent_phreg_quantities(cgd_recurrent_data())
  expect_true(compare_recurrent_phreg(r, sas)$matched)
})

test_that("sas_parity_boundary() lists recurrent PHREG as parity, with the stored run as evidence", {
  b <- sas_parity_boundary()
  expect_true("PROC PHREG (recurrent)" %in% b$proc)
  row <- b[b$proc == "PROC PHREG (recurrent)", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_equal(row$status, "parity")
  expect_true(grepl("SAS 9.4", row$evidence, fixed = TRUE))
  expect_true(grepl("benchmark_recurrent_phreg.lst", row$evidence, fixed = TRUE))
  expect_true(grepl("benchmark_recurrent_phreg.sas", row$evidence, fixed = TRUE))
})

test_that("the stored SAS 9.4 recurrent PHREG listing parses and agrees with R (Tier 1)", {
  lst <- system.file("sas", "list", "benchmark_recurrent_phreg.lst", package = "TempleCBE")
  skip_if(!nzchar(lst), "stored SAS listing not installed")
  sas <- parse_recurrent_phreg_lst(lst)
  # Real SAS output: two fits x two SE tables x three terms.
  expect_equal(nrow(sas), 12)
  expect_setequal(sas$model, c("ag", "pwp_gap"))
  expect_setequal(sas$se_type, c("model", "robust"))
  r <- recurrent_phreg_quantities(cgd_recurrent_data())
  # SAS prints 5 decimals for estimates and standard errors, 4 for
  # p-values and 3 for hazard ratios; the comparison's per-quantity
  # tolerances cover that rounding (see ?compare_recurrent_phreg).
  cmp <- compare_recurrent_phreg(r, sas)
  expect_true(cmp$matched)
})

test_that("parse_recurrent_phreg_lst() tolerates stray non-ASCII bytes in a listing", {
  lst <- synthetic_recurrent_phreg_lst()
  clean <- parse_recurrent_phreg_lst(lst)
  # A Windows SAS session draws PROC FREQ rules with a non-ASCII byte (0x83).
  dirty <- c(
    rawToChar(as.raw(c(0x20, 0x83, 0x83, 0x83))),
    lst
  )
  expect_false(validUTF8(dirty[1]))
  expect_equal(parse_recurrent_phreg_lst(dirty), clean)
})
