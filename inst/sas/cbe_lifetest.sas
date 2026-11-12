/*==============================================================================
  TempleCBE SAS Macro: %cbe_lifetest
  Package: TempleCBE (R & SAS Biostatistics Survival Framework)

  Kaplan-Meier product-limit estimation and the log-rank test, as
  PROC LIFETEST, for parity with TempleCBE's R side
  (km_lifetest_quantities() in R/sas_lifetest_parity.R). This is the first
  increment of broader SAS parity (B5): the R counterpart is survfit() /
  survdiff(), and the comparison is compare_km_lifetest().

  Usage in SAS:
    %let templecbe_sas = <path-to-TempleCBE/inst/sas>;
    %include "&templecbe_sas/cbe_lifetest.sas";
    %cbe_lifetest(data=work.mydata, time=time, status=event, group=arm);

  Parameters:
    data    Input data set (required).
    time    Follow-up time variable (required).
    status  Event indicator, 1 = event, 0 = censored (required).
    group   Optional grouping variable; when given, a log-rank test of
            equality over strata is produced.
    out_km   Output data set for the product-limit estimates
             (default work.cbe_lifetest_km).
    out_quart Output data set for the quartiles / median survival
             (default work.cbe_lifetest_quart).
    out_test Output data set for the test of equality over strata
             (default work.cbe_lifetest_test).

  What is compared with R (see R/sas_lifetest_parity.R):
    * Product-limit survival estimates at each event time, per stratum.
    * Median survival time per stratum.
    * Log-rank chi-square, degrees of freedom and p-value (grouped only).
  Standard errors and confidence limits are NOT compared in this increment:
  SAS prints Greenwood standard errors on a log-log scale by default while
  survfit() defaults differ.

  Can be executed directly from R:
    TempleCBE::run_sas_script(
      TempleCBE::sas_macro_path("benchmark_lifetest_km.sas")
    )
==============================================================================*/

%macro cbe_lifetest(data=, time=, status=, group=,
                    out_km=work.cbe_lifetest_km,
                    out_quart=work.cbe_lifetest_quart,
                    out_test=work.cbe_lifetest_test);
    %if %length(&data) = 0 or %length(&time) = 0 or %length(&status) = 0 %then %do;
        %put ERROR: %cbe_lifetest requires data=, time= and status=.;
        %return;
    %end;

    ods output ProductLimitEstimates=&out_km Quartiles=&out_quart
        %if %length(&group) > 0 %then HomTests=&out_test; ;
    proc lifetest data=&data method=pl plots=none;
        time &time * &status(0);
        %if %length(&group) > 0 %then strata &group;;
    run;
    ods output close;
%mend cbe_lifetest;
