/*==============================================================================
  TempleCBE SAS Validation Benchmark: PROC PHREG on recurrent events
  (Andersen-Gill and Prentice-Williams-Peterson gap time) against
  recurrent_cox() / recurrent_phreg_quantities() (R/sas_phreg_parity.R)

  ----------------------------------------------------------------------------
  STATUS: RUN ONCE on SAS 9.4 (TS1M8, Windows), 12 Nov 2026. Exit status 0,
  no ERROR or WARNING lines. The listing is stored as
  inst/sas/list/benchmark_recurrent_phreg.lst and the log as
  inst/sas/logs/benchmark_recurrent_phreg.log; the Tier-1 test in
  tests/testthat/test-round2-0911-sas.R compares them with the R fits via
  compare_recurrent_phreg(). The synthetic listing used by the other tests
  is R values formatted to look like SAS output; it is NOT SAS output.
  ----------------------------------------------------------------------------

  Data: survival::cgd, the chronic granulomatous disease trial -- 203
  counting-process intervals (tstart, tstop, status) for 128 patients,
  76 recurrent serious infections. The data are NOT typed into this
  program (203 rows): they are exported from R with the package's own
  write_sas_data() and read back here from a SAS transport file (see
  "Getting the data into SAS" below). The export is cgd_recurrent_data()
  in R/sas_phreg_parity.R: id, tstart, tstop, status, treat, sex, age,
  enum, with the two factors recoded to 0/1 so that both sides fit the
  same parameters with the same names:

    treat  1 = rIFN-g, 0 = placebo
    sex    1 = female, 0 = male
    age    age at entry, in years (numeric)
    enum   event number of the interval (1, 2, ...); verified in the
           tests to equal 1 + the number of earlier events of the patient

  Exact model, both fits (the R side is recurrent_phreg_quantities(),
  which fits recurrent_cox() with the same specification):

    (tstart, tstop) * status(0) = treat sex age / ties=efron rl
    id id;                       -- COVS(AGGREGATE) needs the ID statement
    proc phreg ... covs(aggregate) covm;

  * TIES=EFRON, not the SAS default BRESLOW: recurrent_cox() fits with
    Efron ties, so SAS must too. (The R_to_SAS vignette's cox_multi()
    cgd fit uses Breslow and matches SAS Breslow; do not copy that
    option here -- the Efron fit differs in the 4th-5th decimal, e.g.
    the Andersen-Gill treat coefficient is -1.11911 with Efron ties
    against -1.12110 with Breslow.)
  * COVS(AGGREGATE) with ID id requests the Lin-Wei robust sandwich
    covariance, aggregated by patient, which is what recurrent_cox()
    reports: its terms$std_error comes from the clustered (robust)
    covariance. COVM additionally requests the model-based covariance,
    so each fit prints TWO "Analysis of Maximum Likelihood Estimates"
    tables -- one "with Model-Based Variance Estimate" and one "with
    Sandwich Variance Estimate" (the sandwich table has an extra
    StdErr Ratio column). parse_recurrent_phreg_lst() reads both.
  * Andersen-Gill fit: one baseline hazard, total (study) time.
  * PWP gap-time fit: the same model on gap time (time since the
    previous infection) with a baseline hazard per event number,
    STRATA enum3, where enum3 = min(event_no, 3): event numbers 1, 2,
    and 3-or-later, which hold 44, 17 and 15 events. This is the same
    pooling recurrent_cox(model = "pwp_gap", max_events = 3) applies.
    The gap times and event numbers are derived in the DATA step below
    from (tstart, tstop, status) alone, so they do not depend on the
    exported enum column; enum is kept in the export as a cross-check.
  * No %cbe_* macro is needed or used: %cbe_cox_phreg has no COVS or
    STRATA option, so this benchmark calls PROC PHREG directly.

  Getting the data into SAS (run this in R first):

    TempleCBE::write_sas_data(
      TempleCBE::cgd_recurrent_data(),
      "<folder>/cgdrec.xpt"      # file stem at most 8 characters (.xpt rule)
    )

  and put cgdrec.xpt in the same folder as this program
  (inst/sas/cgdrec.xpt when run from the package). The program reads it
  from the folder it was submitted from (the %let script_dir pattern
  benchmark_lifetest_km.sas uses). If you submit this program
  interactively rather than in batch, set script_dir by hand first:

    %let script_dir = <path-to-TempleCBE/inst/sas>/;

  CSV fallback (if the transport file cannot be used): export the same
  data frame with utils::write.csv(..., row.names = FALSE) to
  cgdrec.csv and replace the LIBNAME / DATA step below with

    proc import datafile="&script_dir.cgdrec.csv" out=work.cgd
        dbms=csv replace; getnames=yes; run;

  How to run (SAS 9.4):

    From R (the program, log and listing land beside it, as run_sas_script()
    arranges for the other benchmarks):
      TempleCBE::run_sas_script(
        TempleCBE::sas_macro_path("benchmark_recurrent_phreg.sas")
      )

    Or in SAS directly, in batch:
      sas -sysin <path-to-TempleCBE/inst/sas>/benchmark_recurrent_phreg.sas
          -log <path-to-TempleCBE/inst/sas>/logs/benchmark_recurrent_phreg.log
          -print <path-to-TempleCBE/inst/sas>/list/benchmark_recurrent_phreg.lst

  What to expect: two PROC PHREG runs (Andersen-Gill, then PWP gap time),
  each printing Model Information, the global-null tests (with extra
  Wald (COVM) and Wald (COVS) rows), and the two Maximum Likelihood
  Estimates tables (model-based, then sandwich). Each run also writes an
  ODS ParameterEstimates data set, printed for the record by PROC PRINT
  under an "ODS ParameterEstimates data set:" title. A PROC FREQ of
  status (76 events, 127 censored) prints first, as an import check.

  Where to store the outputs (following the LIFETEST benchmark):

    inst/sas/list/benchmark_recurrent_phreg.lst
    inst/sas/logs/benchmark_recurrent_phreg.log

  After the run: in R, with the listing stored,

    sas <- TempleCBE::parse_recurrent_phreg_lst(
      system.file("sas", "list", "benchmark_recurrent_phreg.lst",
                  package = "TempleCBE")
    )
    r <- TempleCBE::recurrent_phreg_quantities(
      TempleCBE::cgd_recurrent_data()
    )
    TempleCBE::compare_recurrent_phreg(r, sas)

  tests/testthat/test-round2-0911-sas.R has a Tier-1 test in the LIFETEST
  pattern that exercises the stored listing (it skips only if the listing
  is not installed). sas_parity_boundary() lists this as "parity".
==============================================================================*/

/* formchar: plain-ASCII table borders, so the stored listing is portable
   (the Windows default draws PROC FREQ rules with a non-ASCII byte). */
options nodate nonumber linesize=100 pagesize=32767
        formchar="|----|+|---+=|-/\<>*";

/* 1. Locate this program's folder (the pattern benchmark_lifetest_km.sas
   uses), so the transport file beside it can be found. When submitted
   interactively rather than in batch, %sysfunc(getoption(sysin)) is
   blank: set script_dir by hand before running (see the header). */
%let script_dir = %sysfunc(tranwrd(%sysfunc(getoption(sysin)), benchmark_recurrent_phreg.sas, %str()));
%let xpt_path = &script_dir.cgdrec.xpt;

/* 2. Read the transport file written by write_sas_data(cgd_recurrent_data()).
   The transport member name is the file stem, CGDREC. */
libname cgd_xpt xport "&xpt_path";
data work.cgd;
    set cgd_xpt.cgdrec;
run;
libname cgd_xpt clear;

/* Import check: 203 intervals, 76 events, 127 censored. */
title1 "CGD recurrent data check (PROC FREQ)";
proc freq data=work.cgd;
    tables status / nocum;
run;

/* 3. Derive the gap times and the (capped) event number from
   (tstart, tstop, status) alone, in the same way recurrent_cox()
   model = "pwp_gap" does in R: gap time restarts at the previous
   event (0 before the first), and event_no is 1 + the number of
   earlier events of the patient. enum3 pools event numbers 3 and
   above into one stratum (44 / 17 / 15 events). */
proc sort data=work.cgd out=work.cgd_r;
    by id tstart;
run;

data work.cgd_r;
    set work.cgd_r;
    by id;
    retain _prev_event 0 _n_prior 0;
    if first.id then do;
        _prev_event = 0;
        _n_prior = 0;
    end;
    event_no = _n_prior + 1;
    enum3 = min(event_no, 3);
    gap_tstart = tstart - _prev_event;
    gap_tstop = tstop - _prev_event;
    output;
    if status = 1 then do;
        _prev_event = tstop;
        _n_prior = _n_prior + 1;
    end;
    drop _prev_event _n_prior;
run;

/* 4. Andersen-Gill fit: total (study) time, one baseline hazard.
   COVS(AGGREGATE) + ID id gives the robust sandwich standard errors
   that recurrent_cox() reports; COVM adds the model-based table. */
title1 "Recurrent PHREG -- Andersen-Gill (total time): COVS(AGGREGATE) COVM";
ods output ParameterEstimates=work.phreg_ag_est;
proc phreg data=work.cgd_r covs(aggregate) covm;
    model (tstart, tstop)*status(0) = treat sex age / ties=efron rl;
    id id;
run;
ods output close;

title1 "ODS ParameterEstimates data set: Andersen-Gill fit";
proc print data=work.phreg_ag_est noobs;
run;

/* 5. Prentice-Williams-Peterson gap-time fit: gap time since the
   previous infection, baseline hazard per event number (1, 2, 3+). */
title1 "Recurrent PHREG -- PWP gap time: COVS(AGGREGATE) COVM";
ods output ParameterEstimates=work.phreg_gap_est;
proc phreg data=work.cgd_r covs(aggregate) covm;
    model (gap_tstart, gap_tstop)*status(0) = treat sex age / ties=efron rl;
    strata enum3;
    id id;
run;
ods output close;

title1 "ODS ParameterEstimates data set: PWP gap-time fit";
proc print data=work.phreg_gap_est noobs;
run;

title1;
