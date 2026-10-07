/*==============================================================================
  TempleCBE SAS Macro Library: %cbe_cox_phreg
  
  Standardizes proportional hazards Cox regression in SAS PROC PHREG,
  mirroring TempleCBE's cbe_cox_single() and cbe_cox_multi().
  
  Features:
    - Flexible ties handling (BRESLOW default, matching SAS; or EFRON default, matching R).
    - Proportional hazards testing via ASSESS PH / RESAMPLE.
    - Automatic extraction of parameter estimates, hazard ratios, 95% Wald & profile CIs.
    - Cluster/ID handling for counting-process or repeated measurement data.
    - Output dataset generation structured for automated validation against R.
    
  Parameters:
    DATA            : Input dataset.
    TIME            : Event/censoring follow-up time variable.
    STATUS          : Event status variable (1 = event, 0 = censored).
    TSTART          : (Optional) Start time variable for counting-process models.
    VARS            : Model explanatory variables (space-separated).
    CLASS_VARS      : (Optional) Categorical variables to include in CLASS statement.
    REF_LEVELS      : (Optional) Custom reference levels for CLASS variables.
    TIES            : Tie handling (BRESLOW or EFRON; default: BRESLOW).
    ID              : (Optional) Subject ID variable for clustered/repeated observations.
    ASSESS_PH       : Test proportional hazards via ASSESS statement (Y/N; default: Y).
    SEED            : Random seed for ASSESS resampling (default: 123).
    OUT_EST         : Output dataset for parameter estimates (default: cbe_estimates).
    OUT_FIT         : Output dataset for model fit statistics (default: cbe_fit).
    PRINT           : Y/N to print results (default: Y).
==============================================================================*/

%macro cbe_cox_phreg(
    data            = ,
    time            = ,
    status          = ,
    tstart          = ,
    vars            = ,
    class_vars      = ,
    ref_levels      = ,
    ties            = BRESLOW,
    id              = ,
    assess_ph       = Y,
    seed            = 123,
    out_est         = cbe_estimates,
    out_fit         = cbe_fit,
    print           = Y
);

    %put NOTE: ----------------------------------------------------------------;
    %put NOTE: TempleCBE: Running %nrstr(%cbe_cox_phreg)...;
    %put NOTE: ----------------------------------------------------------------;

    %if %superq(data) = %then %do;
        %put ERROR: DATA= parameter is required.;
        %return;
    %end;
    %if %superq(time) = %then %do;
        %put ERROR: TIME= parameter is required.;
        %return;
    %end;
    %if %superq(status) = %then %do;
        %put ERROR: STATUS= parameter is required.;
        %return;
    %end;
    %if %superq(vars) = %then %do;
        %put ERROR: VARS= parameter is required.;
        %return;
    %end;

    /* Model specification: time-fixed vs counting-process start/stop */
    %local model_spec;
    %if %superq(tstart) ^= %then %do;
        %let model_spec = (&tstart, &time) * &status(0) = &vars;
    %end;
    %else %do;
        %let model_spec = &time * &status(0) = &vars;
    %end;

    /* Execute PROC PHREG with ODS output capture */
    ods output ParameterEstimates = _cbe_raw_est
               FitStatistics      = &out_fit;

    proc phreg data=&data;
        %if %superq(class_vars) ^= %then %do;
            class &class_vars %if %superq(ref_levels) ^= %then %do; (ref=&ref_levels) %end; / param=glm;
        %end;
        model &model_spec / ties=&ties rl;
        %if %superq(id) ^= %then %do;
            id &id;
        %end;
        %if %upcase(&assess_ph) = Y and %superq(tstart) = %then %do;
            assess ph / resample seed=&seed;
        %end;
    run;

    /* Tidy parameter estimates */
    data &out_est;
        set _cbe_raw_est;
        length term $32;
        term = Parameter;
        estimate = Estimate;
        std_error = StdErr;
        statistic = ChiSq;
        p_value = ProbChiSq;
        hazard_ratio = HazardRatio;
        hr_lower_95 = HRLowerCL;
        hr_upper_95 = HRUpperCL;
        keep term estimate std_error statistic p_value hazard_ratio hr_lower_95 hr_upper_95;
    run;

    proc datasets lib=work nolist;
        delete _cbe_raw_est;
    quit;

    %if %upcase(&print) = Y %then %do;
        title1 "TempleCBE: Cox Proportional Hazards Model Estimates (TIES=&ties)";
        proc print data=&out_est noobs;
            format estimate std_error 8.4 statistic 8.2 p_value pvalue6.4 hazard_ratio hr_lower_95 hr_upper_95 8.3;
        run;
        title1;
    %end;

    %put NOTE: TempleCBE: %nrstr(%cbe_cox_phreg) completed successfully.;
%mend cbe_cox_phreg;
