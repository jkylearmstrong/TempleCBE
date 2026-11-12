/*==============================================================================
  TempleCBE SAS Validation Benchmark: PROC LIFETEST against
  km_lifetest_quantities() (R/sas_lifetest_parity.R)

  Twelve subjects in two strata, typed into this program (so R and SAS see
  the very same numbers; a test in tests/testthat/test-gaps-0811-w2-data.R
  checks the datalines against the R data frame used by
  km_lifetest_quantities()).

    Stratum A: times 5, 6, 6, 8, 10, 12 with events 1, 1, 0, 1, 1, 0.
      Product-limit survival: 0.8333 at 5, 0.6667 at 6, 0.4444 at 8,
      0.2222 at 10; median 8.
    Stratum B: times 3, 4, 7, 9, 9, 11 with events 1, 1, 1, 0, 1, 0.
      Product-limit survival: 0.8333 at 3, 0.6667 at 4, 0.5000 at 7,
      0.3333 at 9; median 8 (SAS reports the midpoint 8 of the flat step at 0.5).

  The survival and median values above are the hand-computed Kaplan-Meier
  values, verified in R (survfit) by the test file; they are NOT SAS output.
  The log-rank chi-square / p-value are not hand-computed here: compare them
  with compare_km_lifetest() once this program has been run in SAS.

  Status: run once on SAS 9.4 (the log is inst/sas/logs/benchmark_lifetest_km.log,
  the listing inst/sas/list/benchmark_lifetest_km.lst). The listing is parsed
  by parse_lifetest_lst() and compared with km_lifetest_quantities() in
  tests/testthat/test-gaps-0811-w2-data.R. The run showed that SAS prints the
  midpoint (8) as the median of stratum B, whose survival is exactly 0.5 from
  t = 7 to t = 9, and that the ODS table of the product-limit estimates is
  named ProductLimitEstimates in SAS 9.4.

  Can be executed directly from R:
    TempleCBE::run_sas_script(
      TempleCBE::sas_macro_path("benchmark_lifetest_km.sas")
    )
==============================================================================*/

options nodate nonumber linesize=100 pagesize=32767;

/* 1. Load the %cbe_lifetest macro from the directory this program lives in
   (the pattern benchmark_brier_lung.sas uses). */
%let script_dir = %sysfunc(tranwrd(%sysfunc(getoption(sysin)), benchmark_lifetest_km.sas, %str()));
%macro _load_includes;
    %if %length(&script_dir) > 0 %then %do;
        %include "&script_dir/cbe_lifetest.sas";
    %end;
%mend _load_includes;
%_load_includes;

data work.lifetest_km;
    infile datalines dsd missover;
    length group $ 1;
    input group $ time status;
datalines;
A,5,1
A,6,1
A,6,0
A,8,1
A,10,1
A,12,0
B,3,1
B,4,1
B,7,1
B,9,0
B,9,1
B,11,0
;
run;

%cbe_lifetest(data=work.lifetest_km, time=time, status=status, group=group,
              out_km=work.lifetest_km_est, out_quart=work.lifetest_km_quart,
              out_test=work.lifetest_km_test);

/* Printed copies of the ODS output data sets, for the record. */
proc print data=work.lifetest_km_est noobs;
    title "Product-Limit Survival Estimates (ODS ProductLimitEstimates)";
run;
proc print data=work.lifetest_km_quart noobs;
    title "Quartiles (ODS Quartiles)";
run;
proc print data=work.lifetest_km_test noobs;
    title "Test of Equality over Strata (ODS HomTests)";
run;
