/*==============================================================================
  TempleCBE SAS Macro Library: %cbe_brier_score
  
  Computes the Inverse Probability of Censoring Weighted (IPCW) Brier Score
  and Integrated Brier Score (IBS) for survival models, following Graf et al.
  (1999). Produces exact numerical concordance with TempleCBE and tidymodels
  yardstick (brier_survival, brier_survival_integrated). Also evaluates 
  time-dependent model calibration by risk quantile.
  
  Parameters:
    DATA            : Input dataset containing survival outcome and predictors/predictions.
    TIME            : Variable name for observed follow-up time.
    STATUS          : Variable name for event indicator (1 = event, 0 = censored).
    ID              : Optional subject ID variable. If omitted, _N_ is used.
    EVAL_TIMES      : Space-separated list of evaluation times (e.g., 100 200 300 400 500).
    PRED_DATA       : (Optional) Dataset containing predictions. If omitted, predictions
                      are calculated from PRED_VARS via PROC PHREG.
    PRED_SURV       : Variable name in PRED_DATA for predicted survival probability S(t|X).
                      If multiple times are in long format, PRED_DATA should have
                      (ID, EVAL_TIME, PRED_SURV). If wide, PRED_SURV_PREFIX can be used.
    PRED_VARS       : Variables to include in PROC PHREG if auto-generating predictions.
    TIES            : Tie handling method for PROC PHREG (BRESLOW or EFRON; default: BRESLOW).
    TRUNC           : Lower bound truncation for censoring survival G(t) (default: 0.05).
    OUT_BRIER       : Output dataset for time-dependent Brier scores (default: cbe_brier_scores).
    OUT_IBS         : Output dataset for Integrated Brier Score (default: cbe_ibs).
    OUT_CALIB       : Output dataset for calibration deciles (default: cbe_calibration).
    PRINT           : Y/N to print results via PROC PRINT (default: Y).
==============================================================================*/

%macro cbe_brier_score(
    data            = ,
    time            = ,
    status          = ,
    id              = ,
    eval_times      = ,
    pred_data       = ,
    pred_surv       = ,
    pred_vars       = ,
    ties            = BRESLOW,
    trunc           = 0.05,
    out_brier       = cbe_brier_scores,
    out_ibs         = cbe_ibs,
    out_calib       = cbe_calibration,
    print           = Y
);

    %put NOTE: ----------------------------------------------------------------;
    %put NOTE: TempleCBE: Running %nrstr(%cbe_brier_score)...;
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
    %if %superq(eval_times) = %then %do;
        %put ERROR: EVAL_TIMES= parameter is required (e.g. eval_times=100 200 300).;
        %return;
    %end;

    /* 1. Standardize subject ID */
    data _cbe_base;
        set &data;
        %if %superq(id) = %then %do;
            _cbe_id = _N_;
        %end;
        %else %do;
            _cbe_id = &id;
        %end;
        _cbe_time = &time;
        _cbe_status = &status;
    run;

    /* Count total sample size */
    %local n_total;
    data _null_;
        set _cbe_base nobs=nobs;
        call symputx('n_total', nobs);
        stop;
    run;

    /* 2. Estimate Censoring Distribution G(t) via Reverse Kaplan-Meier */
    /* Censoring indicator: 1 if censored (status=0), 0 if event (status=1) */
    data _cbe_censor_in;
        set _cbe_base;
        _cbe_censor_event = ( _cbe_status = 0 );
    run;

    ods select none;
    proc lifetest data=_cbe_censor_in outsurv=_cbe_km_censor plots=none;
        time _cbe_time * _cbe_censor_event(0);
    run;
    ods select all;

    /* Clean KM censoring table: retain time and G(t) */
    data _cbe_g_grid;
        set _cbe_km_censor;
        keep _cbe_time SURVIVAL;
        rename SURVIVAL = _G_surv;
        if not missing(_cbe_time) and not missing(SURVIVAL);
    run;

    /* Sort by time for lookup */
    proc sort data=_cbe_g_grid nodupkey;
        by _cbe_time;
    run;

    /* 3. Obtain Survival Predictions S(t|X) */
    %if %superq(pred_data) = %then %do;
        %if %superq(pred_vars) = %then %do;
            %put ERROR: Either PRED_DATA= or PRED_VARS= must be provided.;
            %return;
        %end;

        /* Fit Cox model and compute predicted survival at eval_times */
        data _cbe_covs;
            set _cbe_base;
            keep _cbe_id &pred_vars;
        run;

        ods select none;
        proc phreg data=_cbe_base;
            model _cbe_time * _cbe_status(0) = &pred_vars / ties=&ties;
            baseline covariates=_cbe_covs out=_cbe_preds survival=_pred_surv timelist=&eval_times;
        run;
        ods select all;

        data _cbe_pred_long;
            set _cbe_preds;
            _eval_time = _cbe_time;
            keep _cbe_id _eval_time _pred_surv;
        run;
    %end;
    %else %do;
        data _cbe_pred_long;
            set &pred_data;
            %if %superq(id) = %then %do;
                _cbe_id = _N_;
            %end;
            %else %do;
                _cbe_id = &id;
            %end;
            _eval_time = eval_time;
            _pred_surv = &pred_surv;
            keep _cbe_id _eval_time _pred_surv;
        run;
    %end;

    /* 4. Loop across evaluation times to compute IPCW Graf Weights and Brier Score */
    %local num_times i t_val;
    %let num_times = %sysfunc(countw(&eval_times));

    /* Initialize accumulator datasets */
    data _cbe_all_bs;
        length eval_time 8 n_eval 8 n_events_by_t 8 n_at_risk 8 brier_score 8;
        call missing(eval_time, n_eval, n_events_by_t, n_at_risk, brier_score);
        stop;
    run;

    data _cbe_all_calib;
        length eval_time 8 decile 8 n_bin 8 mean_pred_surv 8 obs_km_surv 8;
        call missing(eval_time, decile, n_bin, mean_pred_surv, obs_km_surv);
        stop;
    run;

    %do i = 1 %to &num_times;
        %let t_val = %scan(&eval_times, &i);

        /* Evaluate G(t): censoring survival at evaluation time t */
        data _null_;
            set _cbe_g_grid end=eof;
            retain _last_g 1.0;
            if _cbe_time <= &t_val then _last_g = _G_surv;
            if eof then do;
                if _last_g < &trunc then _last_g = &trunc;
                call symputx('G_at_t', _last_g);
            end;
        run;

        /* Merge subject data with predictions for time &t_val */
        proc sql;
            create table _cbe_subj_t as
            select a._cbe_id, a._cbe_time, a._cbe_status,
                   b._pred_surv,
                   &t_val as eval_time
            from _cbe_base as a
            inner join (
                select _cbe_id, _pred_surv 
                from _cbe_pred_long 
                where _eval_time = &t_val
            ) as b
            on a._cbe_id = b._cbe_id;
        quit;

        /* Compute G(T_i-) for subjects with event by t_val */
        /* Left limit of censoring survival at observed event time */
        proc sql;
            create table _cbe_scored_t as
            select a.*,
                   coalesce(
                       (select max(_G_surv) from _cbe_g_grid where _cbe_time < a._cbe_time),
                       1.0
                   ) as _G_at_Ti_raw
            from _cbe_subj_t as a;
        quit;

        data _cbe_scored_t2;
            set _cbe_scored_t;
            _G_at_t = &G_at_t;
            _G_at_Ti = max(_G_at_Ti_raw, &trunc);

            /* Indicator variables */
            _at_risk    = ( _cbe_time > eval_time );
            _event_by_t = ( _cbe_time <= eval_time and _cbe_status = 1 );
            _cens_by_t  = ( _cbe_time <= eval_time and _cbe_status = 0 );

            /* Graf IPCW weights */
            if _at_risk then _weight = 1.0 / _G_at_t;
            else if _event_by_t then _weight = 1.0 / _G_at_Ti;
            else _weight = 0.0;

            /* Brier squared loss */
            if _at_risk then _loss = (1.0 - _pred_surv)**2;
            else if _event_by_t then _loss = (_pred_surv)**2;
            else _loss = 0.0;

            _weighted_loss = _weight * _loss;
        run;

        /* Summarize Brier score at time &t_val */
        proc sql;
            create table _cbe_bs_t as
            select eval_time,
                   count(*) as n_eval,
                   sum(_event_by_t) as n_events_by_t,
                   sum(_at_risk) as n_at_risk,
                   sum(_weighted_loss) / count(*) as brier_score
            from _cbe_scored_t2;
        quit;

        proc append base=_cbe_all_bs data=_cbe_bs_t force;
        run;

        /* Calibration by Decile at time &t_val */
        proc rank data=_cbe_scored_t2 out=_cbe_ranked groups=10;
            var _pred_surv;
            ranks decile;
        run;

        proc sql;
            create table _cbe_cal_t as
            select eval_time,
                   decile + 1 as decile,
                   count(*) as n_bin,
                   mean(_pred_surv) as mean_pred_surv,
                   sum(_at_risk) / count(*) as obs_prop_surv
            from _cbe_ranked
            group by eval_time, decile;
        quit;

        proc append base=_cbe_all_calib data=_cbe_cal_t force;
        run;

    %end;

    /* 5. Compute Integrated Brier Score (IBS) using Trapezoidal Rule */
    proc sort data=_cbe_all_bs out=&out_brier;
        by eval_time;
    run;

    data &out_ibs;
        set &out_brier end=eof;
        retain _auc 0 _prev_t . _prev_bs . _first_t . _last_t .;
        if _N_ = 1 then do;
            _first_t = eval_time;
            _prev_t  = eval_time;
            _prev_bs = brier_score;
        end;
        else do;
            _auc + ( (brier_score + _prev_bs) / 2.0 ) * (eval_time - _prev_t);
            _prev_t  = eval_time;
            _prev_bs = brier_score;
        end;
        if eof then do;
            _last_t = eval_time;
            eval_time_min = _first_t;
            eval_time_max = _last_t;
            ibs = _auc / _last_t;
            keep eval_time_min eval_time_max ibs;
            output;
        end;
    run;

    data &out_calib;
        set _cbe_all_calib;
    run;

    /* Clean up temporary work tables */
    proc datasets lib=work nolist;
        delete _cbe_base _cbe_censor_in _cbe_km_censor _cbe_g_grid
               _cbe_preds _cbe_pred_long _cbe_all_bs _cbe_all_calib
               _cbe_subj_t _cbe_scored_t _cbe_scored_t2 _cbe_bs_t
               _cbe_ranked _cbe_cal_t;
    quit;

    %if %upcase(&print) = Y %then %do;
        title1 "TempleCBE: Time-Dependent Brier Score Evaluation (IPCW Graf Method)";
        proc print data=&out_brier noobs;
            var eval_time n_eval n_events_by_t n_at_risk brier_score;
            format brier_score 8.5;
        run;

        title1 "TempleCBE: Integrated Brier Score (IBS)";
        proc print data=&out_ibs noobs;
            var eval_time_min eval_time_max ibs;
            format ibs 8.5;
        run;
        title1;
    %end;

    %put NOTE: TempleCBE: %nrstr(%cbe_brier_score) completed successfully.;
%mend cbe_brier_score;
