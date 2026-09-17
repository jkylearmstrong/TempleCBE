/*==============================================================================
  TempleCBE SAS Macro Library: %cbe_counting_process
  
  Converts wide-format repeated measurements of a time-varying covariate into
  start/stop counting process intervals (T1, T2, Status, Covariate), exactly
  reproducing the SAS/STAT User's Guide Example 85.7 / 91.7 DATA step algorithm
  and TempleCBE's tidy_tmerge_cox().
  
  Parameters:
    DATA_WIDE       : Wide-format input dataset.
    ID              : Subject ID variable name.
    TIME            : Follow-up / event time variable name.
    DEAD            : Censoring/death indicator (1 = dead/event, 0 = censored).
    OBS_TIMES       : Space-separated list of scheduled observation times.
    VAR_PREFIX      : Column prefix for measurement sequence (default: P, e.g. P1..P15).
    COV_NAME        : Name for the long time-varying covariate (default: Covariate).
    DOSE_VAR        : (Optional) Baseline covariate to retain across intervals (e.g. Dose).
    OUT_DATA        : Output counting process dataset (default: cbe_counting_data).
==============================================================================*/

%macro cbe_counting_process(
    data_wide       = ,
    id              = ID,
    time            = Time,
    dead            = Dead,
    obs_times       = ,
    var_prefix      = P,
    cov_name        = Covariate,
    dose_var        = ,
    out_data        = cbe_counting_data
);

    %put NOTE: ----------------------------------------------------------------;
    %put NOTE: TempleCBE: Running %nrstr(%cbe_counting_process)...;
    %put NOTE: ----------------------------------------------------------------;

    %if %superq(data_wide) = %then %do;
        %put ERROR: DATA_WIDE= parameter is required.;
        %return;
    %end;
    %if %superq(obs_times) = %then %do;
        %put ERROR: OBS_TIMES= parameter is required (e.g. obs_times=27 34 37 41 ...).;
        %return;
    %end;

    %local n_obs;
    %let n_obs = %sysfunc(countw(&obs_times));

    data &out_data;
        set &data_wide;
        array pp[&n_obs] &var_prefix.1 - &var_prefix.&n_obs;
        array qq[&n_obs] &var_prefix.2 - &var_prefix.&n_obs _dummy_;
        array tt[&n_obs] _temporary_ (&obs_times);

        drop _i_ _dummy_ &var_prefix.1 - &var_prefix.&n_obs;
        _dummy_ = .;
        T1 = 0;

        do _i_ = 1 to &n_obs;
            if ( &time < tt[_i_] ) then do;
                T2 = &time;
                &cov_name = pp[_i_];
                Status = &dead;
                output;
                return;
            end;
            else if ( &time = tt[_i_] ) then do;
                T2 = &time;
                &cov_name = qq[_i_];
                Status = &dead;
                output;
                return;
            end;
            else if ( tt[_i_] < &time ) then do;
                if ( pp[_i_] ^= qq[_i_] ) then do;
                    if qq[_i_] = . then T2 = &time;
                    else                T2 = tt[_i_];
                    &cov_name = pp[_i_];
                    Status = 0;
                    output;
                    T1 = T2;
                end;
            end;
        end;

        if ( &time >= tt[&n_obs] ) then do;
            T2 = &time;
            &cov_name = pp[&n_obs];
            Status = &dead;
            output;
        end;
    run;

    %put NOTE: TempleCBE: %nrstr(%cbe_counting_process) successfully created &out_data.;
%mend cbe_counting_process;
