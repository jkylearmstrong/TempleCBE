# Tier 2 parity: run the bundled SAS programs and compare their listings with
# the reference numbers Tier 1 uses (test-sas_parity_reference.R). Needs SAS and
# TEMPLECBE_RUN_SAS_TESTS=true; skipped everywhere else. The comparison is exact
# (as printed): a SAS release that prints something different should be looked at.
# The exceptions are the last two tests, which run generated drivers (decimal
# evaluation or observation times): the first compares its listing with R within
# a tolerance, the second compares two SAS runs with each other and with a table
# worked out by hand.

test_that("SAS reproduces the Example 85.7 reference (live run)", {
  skip_if_no_sas()
  lst <- run_bundled_sas("example_85_7.sas")
  expect_matches_sas(
    parse_phreg_lst(lst, c("Dose", "NPap")),
    sas_reference("sas_phreg_tumor_counting_process.csv")
  )
})

test_that("SAS reproduces the lung Brier/IBS benchmark reference (live run)", {
  skip_if_no_sas()
  lst <- run_bundled_sas("benchmark_brier_lung.sas")
  expect_matches_sas(
    rbind(parse_phreg_lst(lst, c("age", "sex", "ph_karno")), parse_brier_lst(lst)),
    sas_reference("sas_brier_lung31.csv")
  )
})

test_that("SAS reproduces the full-lung Brier/IBS reference, censoring weights included (live run)", {
  skip_if_no_sas()
  lst <- run_bundled_sas("benchmark_brier_lung_full.sas")
  expect_matches_sas(
    rbind(parse_phreg_lst(lst, c("age", "sex", "ph_karno")), parse_brier_lst(lst)),
    sas_reference("sas_brier_lung_full.csv")
  )
  expect_false(
    any(grepl("^WARNING: Variable .* was not found", attr(lst, "log"))),
    info = "the calibration table of %cbe_brier_score must not drop a column"
  )
})

test_that("%cbe_brier_score takes decimal evaluation times as one time each (live run)", {
  skip_if_no_sas()
  skip_if_not_installed("yardstick")
  # Follow-up in years, so 0.5 1 1.5 2 are sensible times. SAS's countw() and
  # %scan() split at "." unless told otherwise, which turned "0.5 1.5 2.5 3" into
  # 0, 5, 1, 5, 2, 5, 3 with no ERROR: out_brier came back with the t = 3 row
  # alone and an IBS of 0. Nothing but a comparison with R notices that.
  lung_days <- lung_full_data()
  lung <- lung_days
  lung$time <- lung$time / 365.25
  eval_times <- c(0.5, 1, 1.5, 2)
  program <- write_brier_driver(lung_days, eval_times, withr::local_tempdir())
  lst <- run_sas_file(program)
  sas <- parse_brier_lst(lst)
  # one row per evaluation time, each once (expect_matches_sas() fails on a
  # missing, an extra or a repeated row) ...
  expect_setequal(sas$term[sas$quantity == "brier_score"], as.character(eval_times))
  # ... holding what R computes from the same model and weights (IBS included).
  # Wider than sas_atol: the listing has 5 decimals (up to 5e-6 of rounding) and
  # SAS stops PHREG at GCONV=1E-8, which moves its Brier scores off R's by up to
  # 3.5e-6 here (0.5 years: SAS 0.190072627, R 0.190076092; full precision from
  # the same run). The tokenising bug moved the numbers by 1e-2 and dropped rows.
  expect_matches_sas(
    brier_quantities(lung_model(lung), lung, eval_times), sas,
    atol = 1e-5
  )
})

test_that("%cbe_counting_process takes decimal obs_times as one time each (live run)", {
  skip_if_no_sas()
  # countw() splits at "." unless told otherwise, so obs_times = 0.5 1 1.5 was
  # counted as 5 times: arrays of 5 with 3 initial values (a WARNING in the log),
  # and every subject who outlived the last time got a missing Covariate in its
  # final row. Whole-number times were never affected, so the same data with all
  # times doubled (obs_times = 1 2 3) is the reference. Doubling is exact in
  # binary floating point, and every time is a multiple of 0.25, so the comparison
  # can be exact.
  obs_times <- c(0.5, 1, 1.5)
  wide <- data.frame(
    id   = 1:6,
    time = c(2, 0.25, 1, 1.25, 3, 0.75),
    dead = c(1, 1, 0, 1, 0, 1),
    P1   = c(5, 3, 4, 2, 4, 1),
    P2   = c(6, NA, 4, 3, 4, 2),
    P3   = c(7, NA, 5, NA, 4, 2)
  )
  doubled <- wide
  doubled$time <- 2 * wide$time
  dir <- withr::local_tempdir()
  lst_decimal <- run_sas_file(write_counting_driver(wide, obs_times, dir, "counting_decimal"))
  lst_whole <- run_sas_file(write_counting_driver(doubled, 2 * obs_times, dir, "counting_whole"))
  decimal <- counting_process_rows(lst_decimal)
  whole <- counting_process_rows(lst_whole)

  # Worked out by hand from the DATA step. A subject who outlives the last time
  # (1 and 5) gets two closing rows, one up to its time and one of length 0 at it
  # (P3 is never equal to the dummy that stands for "P4"), as in Example 85.7;
  # subject 4 stopped being measured after time 1, so its last row has no
  # Covariate; subject 3's time is an observation time, subject 2's precedes all.
  expected <- data.frame(
    id        = c(1, 1, 1, 1, 2, 3, 4, 4, 4, 5, 5, 6, 6),
    T1        = c(0, 0.5, 1, 2, 0, 0, 0, 0.5, 1.25, 0, 3, 0, 0.5),
    T2        = c(0.5, 1, 2, 2, 0.25, 1, 0.5, 1.25, 1.25, 3, 3, 0.5, 0.75),
    Status    = c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1),
    Covariate = c(5, 6, 7, 7, 3, 5, 2, 3, NA, 4, 4, 1, 2)
  )
  expect_equal(whole, transform(expected, T1 = 2 * T1, T2 = 2 * T2))
  # The decimal run has the same rows, in the same order, on the halved scale.
  expect_equal(decimal, expected)
  expect_equal(decimal, transform(whole, T1 = T1 / 2, T2 = T2 / 2))
  for (lst in list(lst_decimal, lst_whole)) {
    expect_false(
      any(grepl("^WARNING", attr(lst, "log"))),
      info = "the arrays of %cbe_counting_process must have one element per observation time"
    )
  }
})

test_that("SAS reproduces the PROC PRINCOMP iris reference (live run)", {
  skip_if_no_sas()
  lst <- run_bundled_sas("benchmark_princomp_iris.sas")
  expect_matches_sas(parse_princomp_lst(lst), sas_reference("sas_princomp_iris.csv"))
})

test_that("SAS reproduces the PROC COMPARE reference, and cbe_compare_df agrees with this run (live run)", {
  skip_if_no_sas()
  lst <- run_bundled_sas("benchmark_proc_compare.sas")
  sas <- parse_compare_lst(lst)
  expect_matches_sas(sas, sas_compare_reference())
  # PROC COMPARE's own warnings about the duplicate ID values of s3_dup, nothing else
  expect_equal(sum(grepl("^WARNING", attr(lst, "log"))), 2L)
  # R against this run, on every scenario but s8_text (a trailing blank; see test-sas_parity_reference.R)
  agree <- setdiff(names(compare_scenarios), "s8_text")
  r <- do.call(rbind, lapply(agree, function(s) compare_quantities(compare_run(s))))
  expect_matches_sas_compare(r, compare_mapped(sas[sas$term %in% agree, ]))
})
