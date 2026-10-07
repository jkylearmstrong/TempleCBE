# Tier 1 parity: R against numbers SAS printed once. They are stored in
# tests/testthat/reference/*.csv and were copied from the listings committed under
# inst/sas/list/, so these tests need no SAS. test-sas_parity_live.R re-runs the
# SAS programs (opt-in) and compares their listings with the same files.

# sas_atol, sas_rtol, lung_full_data() and lung_model() are in helper-sas-reference.R
# (test-sas_parity_live.R uses them too).

lung31_data <- function() {
  lung <- sas_datalines(
    cbe_sas_macro_path("benchmark_brier_lung.sas"),
    c("inst", "time", "status", "age", "sex", "ph_ecog", "ph_karno", "pat_karno", "meal_cal", "wt_loss"),
    csv = TRUE
  )
  # The program's own recode (1 = censored, 2 = dead) and its `delete` of missing ph_karno.
  lung$event <- as.numeric(lung$status == 2)
  lung[!is.na(lung$ph_karno), ]
}

# The rows of a lung reference file that come from %cbe_brier_score, not PHREG.
brier_quantities_of <- function(ref) {
  ref$quantity %in% c("n_events_by_t", "n_at_risk", "brier_score", "ibs")
}

test_that("tumor_wide() holds the data typed into the SAS Example 85.7 program", {
  sas <- sas_datalines(
    cbe_sas_macro_path("example_85_7.sas"),
    c("ID", "Time", "Dead", "Dose", paste0("P", 1:15))
  )
  r <- as.data.frame(tumor_wide())
  expect_named(r, names(sas))
  expect_equal(r, sas, ignore_attr = TRUE)
})

test_that("cbe_cox_multi reproduces SAS's PROC PHREG output for the counting-process tumor data", {
  fit <- cbe_cox_multi(
    tumor_long(), survival::Surv(T1, T2, Status) ~ Dose + NPap,
    id = ID, ties = "breslow"
  )
  expect_true(fit$converged)
  r <- phreg_quantities(fit$model)
  expect_matches_sas(
    r, sas_reference("sas_phreg_tumor_counting_process.csv"),
    atol = sas_atol, rtol = sas_rtol
  )
  # SAS prints the p-value of NPap as <.0001, so the reference has no number to compare.
  expect_lt(r$value[r$quantity == "p" & r$term == "NPap"], 1e-4)
})

test_that("cbe_cox_multi reproduces SAS's PROC PHREG output on the 31-subject lung benchmark", {
  lung <- lung31_data()
  expect_equal(c(nrow(lung), sum(lung$event)), c(31, 28))
  ref <- sas_reference("sas_brier_lung31.csv")
  expect_matches_sas(
    phreg_quantities(lung_model(lung)),
    ref[!brier_quantities_of(ref), ],
    atol = sas_atol, rtol = sas_rtol
  )
})

test_that("R reproduces SAS's Brier scores and IBS on the 31-subject lung benchmark (all censoring weights 1)", {
  skip_if_not_installed("yardstick")
  lung <- lung31_data()
  eval_times <- c(100, 200, 300, 400, 500)
  # All three censoring times (705, 1010, 1022) are after the last evaluation time,
  # so every censoring weight is 1: this benchmark checks the Cox baseline survival
  # and the squared loss, not the weights. test-surv_helpers.R covers those.
  expect_true(all(lung$time[lung$event == 0] > max(eval_times)))
  ref <- sas_reference("sas_brier_lung31.csv")
  expect_matches_sas(
    brier_quantities(lung_model(lung), lung, eval_times),
    ref[brier_quantities_of(ref), ],
    atol = sas_atol
  )
})

test_that("benchmark_brier_lung_full.sas holds the rows of survival::lung", {
  sas <- sas_datalines(
    cbe_sas_macro_path("benchmark_brier_lung_full.sas"),
    c("inst", "time", "status", "age", "sex", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss"),
    csv = TRUE
  )
  expect_equal(sas, as.data.frame(survival::lung), ignore_attr = TRUE)
})

test_that("cbe_cox_multi reproduces SAS's PROC PHREG output on the full lung cohort", {
  lung <- lung_full_data()
  expect_equal(c(nrow(lung), sum(lung$event)), c(227, 164))
  ref <- sas_reference("sas_brier_lung_full.csv")
  r <- phreg_quantities(lung_model(lung))
  ref <- ref[!brier_quantities_of(ref), ]
  # The log-likelihood based rows move only at second order with the estimates
  # and SAS prints them with 3 decimals (half a unit: 5e-4).
  fit <- function(x) x$term == "model" & x$quantity != "wald_chisq"
  expect_matches_sas(r[fit(r), ], ref[fit(ref), ], atol = 6e-4)
  # The estimates and what is computed from them get wider bounds than
  # sas_atol/sas_rtol: SAS stops at GCONV=1E-8, which on this larger cohort
  # leaves its estimates about 2.5e-4 (relative) off the maximum that R's coxph
  # reaches (rerun with GCONV=1E-14, SAS printed R's Wald chi-square 18.6825 and
  # sex chi-square 8.7676, not 18.695 and 8.7714), and SAS prints p-values and
  # chi-squares with 4 decimals.
  expect_matches_sas(r[!fit(r), ], ref[!fit(ref), ], atol = 6e-5, rtol = 1.5e-3)
})

test_that("R reproduces SAS's Brier scores and IBS on the full lung cohort (censoring weights matter)", {
  skip_if_not_installed("yardstick")
  lung <- lung_full_data()
  eval_times <- c(100, 200, 300, 400, 500)
  # 49 subjects are censored by day 500, so, unlike the 31-subject benchmark, the
  # censoring weights G(t) and G(T-) differ from 1. SAS printed 0.11248 0.19918
  # 0.22247 0.21612 0.18977 (IBS 0.15778) while %cbe_brier_score took G(T-) as 1.
  expect_equal(sum(lung$event == 0 & lung$time <= max(eval_times)), 49)
  ref <- sas_reference("sas_brier_lung_full.csv")
  expect_matches_sas(
    brier_quantities(lung_model(lung), lung, eval_times),
    ref[brier_quantities_of(ref), ],
    atol = sas_atol
  )
})

test_that("expect_matches_sas fails on a wrong, a missing or an unreferenced number", {
  ref <- sas_reference("sas_brier_lung31.csv")
  wrong <- ref
  third <- which(wrong$quantity == "brier_score")[3]
  wrong$value[third] <- wrong$value[third] + 1e-4
  expect_failure(expect_matches_sas(wrong, ref, atol = sas_atol), "brier_score")
  expect_failure(expect_matches_sas(ref[-third, ], ref, atol = sas_atol), "brier_score")
  # a row deleted from the reference must not go unnoticed either
  expect_failure(expect_matches_sas(ref, ref[-third, ], atol = sas_atol), "brier_score")
  # nor a row listed twice (the old macro printed one Brier row per subject)
  expect_failure(expect_matches_sas(rbind(ref, ref[third, ]), ref, atol = sas_atol), "brier_score")
})

test_that("the reference files hold what the committed SAS listings show", {
  lst_dir <- file.path(cbe_sas_macro_dir(), "list")
  tumor <- readLines(file.path(lst_dir, "example_85_7.lst"))
  ref <- sas_reference("sas_phreg_tumor_counting_process.csv")
  expect_matches_sas(parse_phreg_lst(tumor, c("Dose", "NPap")), ref)
  expect_equal(sum(is.na(ref$value)), 1) # the "<.0001" p-value of NPap

  lung <- readLines(file.path(lst_dir, "benchmark_brier_lung.lst"))
  ref <- sas_reference("sas_brier_lung31.csv")
  expect_matches_sas(
    rbind(parse_phreg_lst(lung, c("age", "sex", "ph_karno")), parse_brier_lst(lung)),
    ref
  )
  expect_false(anyNA(ref$value))
  expect_equal(sum(ref$quantity == "brier_score"), 5) # one score per evaluation time

  full <- readLines(file.path(lst_dir, "benchmark_brier_lung_full.lst"))
  ref <- sas_reference("sas_brier_lung_full.csv")
  expect_matches_sas(
    rbind(parse_phreg_lst(full, c("age", "sex", "ph_karno")), parse_brier_lst(full)),
    ref
  )
  expect_false(anyNA(ref$value))
  expect_equal(sum(ref$quantity == "brier_score"), 5)
})

test_that("the committed SAS logs of the Brier programs have no error and no unset-variable warning", {
  log_dir <- file.path(cbe_sas_macro_dir(), "logs")
  for (stem in c("benchmark_brier_lung", "benchmark_brier_lung_full")) {
    log <- readLines(file.path(log_dir, paste0(stem, ".log")))
    expect_false(any(grepl("^ERROR", log)), info = stem)
    # obs_prop_surv/obs_km_surv: the calibration table of %cbe_brier_score once
    # appended a column its base table did not have, at every evaluation time
    expect_false(any(grepl("^WARNING: Variable .* was not found", log)), info = stem)
  }
})

test_that("benchmark_princomp_iris.sas holds the rows of datasets::iris", {
  sas <- sas_datalines(
    cbe_sas_macro_path("benchmark_princomp_iris.sas"),
    c("sepal_length", "sepal_width", "petal_length", "petal_width", "species"),
    csv = TRUE
  )
  expect_equal(nrow(sas), 150)
  expect_equal(sas$sepal_length, datasets::iris$Sepal.Length)
  expect_equal(sas$sepal_width, datasets::iris$Sepal.Width)
  expect_equal(sas$petal_length, datasets::iris$Petal.Length)
  expect_equal(sas$petal_width, datasets::iris$Petal.Width)
  expect_equal(as.character(sas$species), as.character(datasets::iris$Species))
})

test_that("proc_pca reproduces the eigenvalue table of SAS PROC PRINCOMP on the iris data", {
  ref <- sas_reference("sas_princomp_iris.csv")
  # four components, the last without a difference
  expect_equal(c(table(ref$quantity)), c(cumulative = 4L, difference = 3L, eigenvalue = 4L, proportion = 4L))
  expect_false(anyNA(ref$value))
  res <- proc_pca(datasets::iris, scale = TRUE)
  expect_matches_sas_princomp(princomp_quantities(res), ref)
})

test_that("the PROC PRINCOMP reference file holds what the committed listing shows, and the log is clean", {
  lst_dir <- file.path(cbe_sas_macro_dir(), "list")
  lst <- readLines(file.path(lst_dir, "benchmark_princomp_iris.lst"))
  expect_matches_sas(parse_princomp_lst(lst), sas_reference("sas_princomp_iris.csv"))

  log <- readLines(file.path(cbe_sas_macro_dir(), "logs", "benchmark_princomp_iris.log"))
  expect_false(any(grepl("^(ERROR|WARNING)", log)))
  expect_true(any(grepl("WORK.IRIS has 150 observations", log)))
})

test_that("the PROC PRINCOMP tolerances catch a number that is off by more than SAS rounds", {
  ref <- sas_reference("sas_princomp_iris.csv")
  fine <- ref[ref$quantity %in% sas_princomp_fine, ]
  coarse <- ref[!ref$quantity %in% sas_princomp_fine, ]
  # within the rounding of what SAS printed: accepted
  ok <- fine
  ok$value <- ok$value + 4e-9
  expect_matches_sas(ok, fine, atol = sas_princomp_atol[["fine"]])
  # one unit in the 8th decimal: rejected
  wrong <- fine
  wrong$value[wrong$quantity == "eigenvalue" & wrong$term == "2"] <- 0.91403048
  expect_failure(expect_matches_sas(wrong, fine, atol = sas_princomp_atol[["fine"]]), "eigenvalue")
  # one unit in the 4th decimal: rejected
  wrong <- coarse
  wrong$value[wrong$quantity == "proportion" & wrong$term == "1"] <- 0.7297
  expect_failure(expect_matches_sas(wrong, coarse, atol = sas_princomp_atol[["coarse"]]), "proportion")
})

# --- PROC COMPARE (benchmark_proc_compare.sas) vs cbe_compare_df() ---------------------

compare_sas_path <- function() cbe_sas_macro_path("benchmark_proc_compare.sas")

# one number of the reference
compare_ref <- function(scenario, statistic) {
  ref <- sas_compare_reference()
  ref$value[ref$term == scenario & ref$quantity == statistic]
}

test_that("benchmark_proc_compare.sas holds the data of the R fixtures and calls PROC COMPARE as cbe_compare_df() is called", {
  path <- compare_sas_path()
  fixtures <- compare_fixtures()
  for (name in names(fixtures)) {
    expect_equal(sas_dataset(path, name), fixtures[[name]], info = name)
  }
  program <- readLines(path)
  # s6_same compares s1_base with this copy of it; s1_comp is typed unsorted and sorted by SAS
  heads <-sub("^data work\\.(\\w+);.*$", "\\1", grep("^data work\\.\\w+;", program, value = TRUE))
  expect_setequal(heads, c(names(fixtures), "s6_comp"))
  expect_true(any(grepl("^\\s*set work\\.s1_base;", program)))
  expect_true(is.unsorted(fixtures$s1_comp$id))
  expect_true(any(grepl("^proc sort data=work\\.s1_comp;", program)))
  # one call per scenario: METHOD=ABSOLUTE, CRITERION = tolerance, ID statement = by
  expect_equal(sum(grepl("^proc compare .* method=absolute criterion=", program)), length(compare_scenarios))
  calls <- sas_compare_calls(path)
  expect_equal(calls$scenario, names(compare_scenarios))
  spec <- function(field) vapply(compare_scenarios, function(s) if (is.null(s[[field]])) "" else s[[field]], "")
  expect_equal(calls$criterion, unname(vapply(compare_scenarios, function(s) s$tolerance, 0)))
  expect_equal(calls$id, unname(spec("by")))
  expect_equal(ifelse(calls$compare == "s6_comp", "s1_base", calls$compare), unname(spec("compare")))
  expect_equal(calls$base, unname(spec("base")))
})

test_that("cbe_compare_df agrees with SAS PROC COMPARE on every statistic they both report", {
  ref <- sas_compare_reference()
  expect_setequal(ref$term, names(compare_scenarios))
  # s8_text differs in one respect, pinned in its own test below
  agree <- setdiff(names(compare_scenarios), "s8_text")
  r <- do.call(rbind, lapply(agree, function(s) compare_quantities(compare_run(s))))
  sas <- compare_mapped(ref[ref$term %in% agree, ])
  expect_gt(nrow(sas), 200)
  expect_matches_sas_compare(r, sas)
})

test_that("every statistic of the PROC COMPARE reference is mapped to cbe_compare_df() or listed as unmapped", {
  ref <- sas_compare_reference()
  expect_setequal(compare_family(ref$quantity), compare_statistic_map$statistic)
  expect_false(anyDuplicated(compare_statistic_map$statistic) > 0)
  expect_setequal(
    compare_family(compare_quantities(compare_run("s1_id"))$quantity),
    setdiff(compare_statistic_map$statistic, compare_statistics_unmapped)
  )
})

test_that("the SYSINFO bits PROC COMPARE sets, where cbe_compare_df has a counterpart, say what it says", {
  for (s in names(compare_scenarios)) {
    sas <- bitwAnd(as.integer(compare_ref(s, "sysinfo")), compare_sysinfo_mask)
    run <- compare_run(s)
    expect_equal(compare_sysinfo_r(run), sas, info = s)
    expect_equal(run$cmp$is_concordant, sas == 0L, info = s)
  }
  # the bits it has none for: s1_id has a character variable of length 8 and 12 (bit 16)
  expect_equal(bitwAnd(as.integer(compare_ref("s1_id", "sysinfo")), 63L), 16L)
})

test_that("the PROC COMPARE reference file holds what the committed listing shows, and the log is clean", {
  lst <- readLines(file.path(cbe_sas_macro_dir(), "list", "benchmark_proc_compare.lst"))
  expect_matches_sas(parse_compare_lst(lst), sas_compare_reference())
  # a count SAS leaves out is read as 0, and so is a whole Values Comparison Summary
  # when all values are exactly equal
  s6 <- lst[grep("Scenario s6_same", lst)[1]:grep("SYSINFO s6_same", lst)[1]]
  expect_true(any(grepl("No unequal values were found. All values compared are exactly equal", s6)))
  expect_false(any(grepl("Values Comparison Summary|Maximum Difference", s6)))
  expect_false(any(grepl("but not in", s6)))

  log <- readLines(file.path(cbe_sas_macro_dir(), "logs", "benchmark_proc_compare.log"))
  expect_false(any(grepl("^ERROR", log)))
  # PROC COMPARE's own warnings about the duplicate ID values of s3_dup (and nothing else)
  warnings <- grep("^WARNING", log, value = TRUE)
  expect_length(warnings, 2L)
  expect_true(all(grepl("contains a duplicate observation at observation number 2", warnings)))
  expect_true(any(grepl("WORK.S1_BASE has 10 observations", log)))
  expect_false(any(grepl("Site +[0-9]", log)))
})

test_that("the PROC COMPARE comparison fails when cbe_compare_df is run differently", {
  ref <- sas_compare_reference()
  sas <- function(s) compare_mapped(ref[ref$term == s, ])
  # a tolerance 10000 times the criterion (s1_id has differences of 1e-6 and 0.001)
  expect_failure(
    expect_matches_sas_compare(compare_quantities(compare_run("s1_id", tolerance = 1e-3)), sas("s1_id")),
    "values_unequal"
  )
  # one variable dropped from COMPARE
  fixtures <- compare_fixtures()
  fixtures$s1_comp$over_tol <- NULL
  expect_failure(
    expect_matches_sas_compare(compare_quantities(compare_run("s1_id", fixtures)), sas("s1_id")),
    "vars_common"
  )
  # one count off by one, one difference off in what SAS printed (accepted: within its rounding)
  counts <- sas("s1_id")
  wrong <- counts
  wrong$value[wrong$quantity == "obs_common"] <- 9
  expect_failure(expect_matches_sas_compare(wrong, counts), "obs_common")
  wrong <- counts
  wrong$value[wrong$quantity == "maxdif[over_tol]"] <- 2.51
  expect_failure(expect_matches_sas_compare(wrong, counts), "maxdif")
  wrong$value[wrong$quantity == "maxdif[over_tol]"] <- 2.5 + 1e-4
  expect_matches_sas_compare(wrong, counts)
})

# --- where cbe_compare_df() and PROC COMPARE define something differently -------------
# (the SAS numbers are those of the committed listing, in reference/sas_proc_compare.csv)

test_that("a difference is base minus compare in cbe_compare_df, compare minus base in PROC COMPARE", {
  d <- compare_run("s1_id")$cmp$diffs
  d3 <- d[d$variable == "over_tol" & d$id == 3, ]
  # s1_id, id 3: Base 7, Compare 7.001; SAS lists Diff. 0.001000
  expect_equal(c(d3$base_value, d3$compare_value), c("7", "7.001"))
  expect_equal(compare_ref("s1_id", "diff[over_tol@3]"), 0.001)
  expect_equal(d3$diff, -0.001)
})

test_that("a variable of different types is compared as text by cbe_compare_df, not at all by PROC COMPARE", {
  cmp <- compare_run("s1_id")$cmp
  conf <- cmp$summary[cmp$summary$variable == "conf", ]
  expect_equal(c(conf$type_base, conf$type_compare, conf$types_match), c("numeric", "character", FALSE))
  # SAS: one conflicting type, conf is not compared (no ndif[conf]); 9 unequal values in all.
  expect_equal(compare_ref("s1_id", "vars_conflicting_type"), 1)
  ref <- sas_compare_reference()
  expect_false("ndif[conf]" %in% ref$quantity[ref$term == "s1_id"])
  expect_equal(compare_ref("s1_id", "values_unequal"), 9)
  # R compares 2 with "2.0" as text: one more unequal value (and the data are not concordant)
  expect_equal(conf$n_diff, 1)
  expect_equal(sum(cmp$summary$n_diff), 10)
  expect_false(cmp$is_concordant)
})

test_that("types are R classes: identical values of class integer and numeric, or factor and character, are no match", {
  # SAS has only numeric and character, so PROC COMPARE would report both pairs as equal (not run).
  cmp <- cbe_compare_df(data.frame(id = 1:2, n = 1:2), data.frame(id = 1:2, n = c(1, 2)), by = "id")
  fct <- cbe_compare_df(
    data.frame(id = 1:2, s = c("a", "b")), data.frame(id = 1:2, s = factor(c("a", "b"))), by = "id"
  )
  for (res in list(cmp, fct)) {
    expect_equal(res$summary$n_diff, 0)
    expect_false(res$summary$types_match)
    expect_false(res$is_concordant)
  }
})

test_that("a trailing blank is a difference to cbe_compare_df and none to PROC COMPARE (s8_text)", {
  ref <- sas_compare_reference()
  sas <- compare_mapped(ref[ref$term == "s8_text", ])
  r <- compare_quantities(compare_run("s8_text"))
  # base "def", compare "def  " (id 2) and "ghi" against "GHI" (id 3): SAS pads the shorter value, so
  # only the case differs there; R compares the strings as they are
  differing <- c("obs_some_unequal", "obs_all_equal", "values_unequal", "values_not_exact", "ndif[s]")
  expect_equal(sas$value[match(differing, sas$quantity)], c(1, 3, 1, 1, 1))
  expect_equal(r$value[match(differing, r$quantity)], c(2, 2, 2, 2, 2))
  expect_false("unequal[s@2]" %in% sas$quantity)
  expect_true("unequal[s@2]" %in% r$quantity)
  expect_matches_sas_compare(
    r[!r$quantity %in% c(differing, "unequal[s@2]"), ],
    sas[!sas$quantity %in% differing, ]
  )
})

test_that("cbe_compare_df has no missing character value other than NA, and compares nothing but type and values", {
  # SAS's missing character value is the blank; a blank is equal to a blank (s1_id, id 9) and
  # different from a value (id 5 and 8: SAS lists them). R: "" is a value; NA differs from "".
  cmp <- cbe_compare_df(
    data.frame(id = 1:3, s = c(NA, "", "a")), data.frame(id = 1:3, s = c("", "", "a")), by = "id"
  )
  expect_equal(cmp$diffs$id, 1)
  expect_equal(compare_ref("s1_id", "ndif[chr_blank]"), 2)
  # Attributes: SAS reports chr_len (length 8 in BASE, 12 in COMPARE) as a variable with differing
  # attributes; cbe_compare_df has no length (nor format or informat) to report, and a label only of
  # BASE, so different labels are no difference to it.
  expect_equal(compare_ref("s1_id", "vars_differing_attr"), 1)
  s <- compare_run("s1_id")$cmp$summary
  expect_true(s$types_match[s$variable == "chr_len"])
  expect_equal(s$n_diff[s$variable == "chr_len"], 0)
  x <- data.frame(id = 1:2, v = c(1, 2))
  y <- x
  attr(x$v, "label") <- "Weight"
  attr(y$v, "label") <- "Mass"
  expect_true(cbe_compare_df(x, y, by = "id")$is_concordant)
})

test_that("rows with the same ID are paired in order, with a warning, as PROC COMPARE does (s3_dup)", {
  run <- compare_run("s3_dup")
  expect_true(run$duplicate_warning)
  expect_false(any(vapply(setdiff(names(compare_scenarios), "s3_dup"), function(s) compare_run(s)$duplicate_warning, NA)))
  # SAS: observation 3 of COMPARE (id 1) and observation 6 of BASE (id 3) have no partner; BASE
  # observation 5 (31) is paired with COMPARE observation 6 (99), the only unequal pair.
  d <- run$cmp$diffs
  expect_equal(c(d$row_base, d$row_compare, d$base_value, d$compare_value), c(5, 6, "31", "99"))
  expect_equal(c(run$cmp$observations$unmatched_base, run$cmp$observations$unmatched_compare), c(1, 1))
  # SAS counts the ID values that occur again
  fixtures <- compare_fixtures()
  expect_equal(compare_ref("s3_dup", "dup_obs_base"), sum(duplicated(fixtures$s3_base$id)))
  expect_equal(compare_ref("s3_dup", "dup_obs_compare"), sum(duplicated(fixtures$s3_comp$id)))
})

test_that("cbe_compare_df needs no sorted data, PROC COMPARE does (s1_id)", {
  fixtures <- compare_fixtures()
  sorted <- fixtures$s1_comp[order(fixtures$s1_comp$id), ]
  cmp <- cbe_compare_df(fixtures$s1_base, sorted, by = "id", base_name = "base", compare_name = "compare")
  expect_equal(cmp$summary, compare_run("s1_id")$cmp$summary)
  expect_equal(cmp$observations, compare_run("s1_id")$cmp$observations)
})

test_that("a difference of exactly the tolerance is none, and decimal fractions are not exact, in both", {
  # s4_edge, tolerance 0.25 (exact in binary): ids 1 and 4 differ by exactly +-0.25 and are not listed
  # by SAS; ids 2, 5, 6 (by 0.2500001, 0.5, -0.2500001) and 7 (0.25 plus 2e-15) are.
  d4 <- compare_run("s4_edge")$cmp$diffs
  expect_equal(d4$id, c(2, 5, 6, 7))
  expect_equal(abs(compare_fixtures()$s4_comp$x[c(1, 4)] - 1), c(0.25, 0.25))
  # s9_trap, tolerance 0.1: 1.1 - 1 is 0.1 plus 9e-17 (listed by SAS), 0.6 - 0.5 is 0.1 minus 3e-17
  # (not listed), so "exactly 0.1" is a difference for one pair and none for the other
  expect_equal(compare_run("s9_trap")$cmp$diffs$id, 1)
  expect_equal(compare_ref("s9_trap", "values_unequal"), 1)
  expect_equal(compare_ref("s9_trap", "values_not_exact"), 3)
  # s7_within: every difference (5e-8) is inside the criterion: no unequal value, concordant,
  # and the maximum difference is still reported (SAS: 5E-8)
  cmp <- compare_run("s7_within")$cmp
  expect_true(cmp$is_concordant)
  expect_equal(max(cmp$summary$max_diff), 5e-8)
  expect_equal(compare_ref("s7_within", "max_diff"), 5e-8)
})
