
/******************************************************************************
* Macro: coxtvc
* Description: Compute survival estimates for a Cox Proportional Hazards model
*              in the presence of time-varying coefficients.
*
* Parameters:
*   The following parameters define the data used to fit the model and create
*   estimates. Consider a SAS dataset with all variables of interest. We can
*   partition this dataset into the following key types of variables:
*      (A) Response variables. These consist of the variables that indicate
*            the beginning and end of the time interval to which the record
*            corresponds. These variables MUST be present. If censoring is
*            an issue, a variable indicating censoring is present.
*      (B) Predictors to be used in the model that have no time-varying
*            coefficients.
*      (C) Predictors to be used in the model that have time-varying
*            coefficients (usually to account for a violation in the
*            proportional hazards assumption).
*      (D) Predictors that account for the time-varying coefficients in (D).
*
*   This is best seen in an example. Suppose we want to obtain survival
*   estimates for mortality. The SAS dataset SURV contains all relevant
*   variables. The variable DEATH takes on a value of 1 if the subject dies
*   and 0 if the subject was censored. As SURV expresses the data in a counting
*   style process, there are multiple records per patient, each record
*   corresponding to a different time interval, beginning at TTDEATH0 and
*   ending at TTDEATH1. The predictors of interest are AGE, GENDER, SBP, and
*   BMI. We know that the proportional hazards assumption is violated for AGE.
*   Thus, we define the variable TM_AGE which contains the interaction of AGE
*   with the event time to account for this violation in the the PH assumption.
*   Then, the variables are divided into:
*      (A) TTDEATH0 TTDEATH1 DEATH
*      (B) GENDER SBP BMI
*      (C) AGE
*      (D) TM_AGE
*
*   data       SAS dataset containing all necessary variables. This dataset
*                should be in the counting-process style of input. See the
*                PHREG documentation for examples of the counting-process
*                style and the macro %CPDATA() for converting a SAS dataset
*                into the counting-process style.
*   y          The response for the survival model as specified for the
*                counting-process style of input. Most often, of the form
*                        y=(TTDEATH0, TTDEATH1)*DEATH(0)
*                See PHREG documentation for a full discussion on specifying
*                the response for the counting-process style of input.
*   x          The list of predictors that appear in the MODEL statement within
*                a call to PHREG. Each variable is separated by a space.
*   tvvar      The list of predictors that have time-varying coefficients
*                (corresponding to (C) above). Note that each variable listed
*                in PHVAR may not necessarily be in X, depending on how the
*                model is parameterized. See the examples below for more
*                details.
*   nontvvar   The list of predictors that do not have time-varying
*                coefficients (corresponding to (B) above). These variables
*                MUST appear in X as well.
*
*   The following parameters address information needed to obtain survival
*   estimates. Survival estimates are produced for a specified set of
*   covariate values. These estimates are produced using the coefficient
*   estimates from a fitted Cox PH model.
*
*   covs       SAS dataset containing covariates at which to estimate
*                survival. This dataset should contain values for
*                variables listed in X1 and X2 (corresponding to variables of
*                type (B) and (C) above). Variables not specified in COVS
*                dataset will be set to their average values if the variable
*                is numeric and their reference values if categorical. CLASS
*                variables are determined as in PHREG when BASELINE statement
*                is used. However, averages are calculated based on one-record
*                per-patient, which differs from PHREG calculations. Therefore,
*                check output carefully to ensure it is what you expected.
*   ests       SAS dataset containing the estimates from the fitted model.
*                If unspecified, the survival model is fit to obtain these
*                estimates. Depending on the complexity of the model, this
*                fitting procedure could be time-costly. See PHREG
*                documentation on the INEST= option within the PHREG statement
*                for more details on specifying this dataset.
*
*   The following parameters are optional. Each governs the options used in
*   fitting the survival model and/or in obtaining survival estimates. It is
*   better to fit the model externally and obtain the ESTS dataset to avoid
*   possible complications from using these parameters.
*
*   modopts    Model options to use in fitting the model that are specified
*                after a / in the MODEL statement of PHREG. (ignored if ests
*                is specified above). This should be enclosed in %STR() to
*                ensure proper evaluation.
*   procopts   Options to use in the PROC PHREG statement in fitting the
*                model (ignored if ests is specified above). This should be
*                enclosed in %STR() to ensure proper evaluation.
*   addstmts   Additional statements that should be included in fitting the
*                model and generating survival estimates. Usually, these will
*                be restricted to the FREQ statement, WEIGHT statement, and
*                possibly CLASS statement. This should be enclosed in %STR()
*                to ensure proper evaluation.
*
*   The following parameters determine how output is displayed and retrieved.
*
*   out        Name of a SAS dataset to create that will store the resulting
*                survival estimates (default=SurvEsts).
*
*   In addition to the above parameters, the macro %VARDEFN must be defined
*   by the user prior to a call to %COXTVC. This macro contains the
*   processing statements used to create the variables that account for the
*   time-varying coefficients (corresponding to the variables in (D) above).
*   All variable definitions go inside the %VARDEFN macro as if they were
*   encountered in a datastep. For example:
*      %macro vardefn;
*          TM_AGE = AGE * TTDEATH1;
*          %mend;
*   See the following sections for more examples involving this definition.
*
* Notes:
*   1. Requires SAS 9 or later.
*   2. The following datasets are created within the macro:
*            _ests_, _means_, _temp_covs_, _unique_
*        To avoid possible problems, these datasets should not be defined in
*        the current work directory, as they will be overwritten.
*
* Results:
* Usage:
*   Example 1: Basic Usage
*     Consider the excert from the following SAS dataset SURV.
*            SUBJ   TTDEATH0  TTDEATH1  DEATH  AGE  GENDER AGE_LT7 AGE_GE7
*            1      0          1        1      21   0        21       0
*            2      0          1        0      25   1        25       0
*            2      4          7        0      25   1         0      25
*            3      0          1        0      20   0        20       0
*            3      4          7        1      20   0         0      20
*            4      0          1        0      21   1        21       0
*            4      4          7        0      21   1         0      21
*            4      7         10        1      21   1         0      21
*     AGE_LT7 and AGE_GE7 are used to capture the thought that the effect of
*     AGE is different prior to day 7 than it is following day 7. These are
*     created using the following code:
*            AGE_LT7 = (TTDEATH1 < 7) * AGE
*            AGE_GE7 = (TTDEATH1 >= 7) * AGE
*     Note that ALL time-varying coefficient variables should be defined using
*     the time variable defining the end of the interval (TTDEATH1).
*
*     To obtain survival estimates for a females that are 20 and 21 years old,
*     we run the following code:
*            %macro vardefn;
*                AGE_LT7 = (TTDEATH1 < 7) * AGE;
*                AGE_GE7 = (TTDEATH1 >= 7) * AGE;
*                %mend;
*
*            %coxtvc(data = SURV,
*                    y = (TTDEATH0, TTDEATH1)*DEATH(0),
*                    x = GENDER AGE_LT7 AGE_GE7,
*                    tvvar = AGE,
*                    nontvvar = GENDER,
*                    covs = COVS,
*                    ests = ESTS);
*     where COVS and ESTS are created by
*           data COVS;
*               input AGE GENDER;
*               datalines;
*               20 0
*               21 0
*               ;
*               run;
*
*           proc phreg data = SURV outest = ESTS;
*               model (TTDEATH0, TTDEATH1)*DEATH(0) = GENDER AGE_LT7 AGE_GE7;
*               run;
*
*     We observe that the statements in the VARDEFN macro definition
*     correspond to those used to create the variables in a DATA STEP exactly.
*     We also note that AGE appeared in the TVVAR parameter even though it is
*     not listed in the MODEL statement in PHREG (as seen when creating ESTS).
*
*
*   Example 2: Example 1 Revisited (Another Parameterization)
*     Consider the data described in Example 1. Consider the following
*     parameterization to accomplish the same task.
*            SUBJ   TTDEATH0  TTDEATH1  DEATH  AGE  GENDER AGE_GE7
*            1      0          1        1      21   0         0
*            2      0          1        0      25   1         0
*            2      4          7        0      25   1        25
*            3      0          1        0      20   0         0
*            3      4          7        1      20   0        20
*            4      0          1        0      21   1         0
*            4      4          7        0      21   1        21
*            4      7         10        1      21   1        21
*     Instead of using AGE_LT7 and AGE_GE7 as before, we capture the same
*     effect using AGE and AGE_GE7. We obtain the same estimates using
*            %macro vardefn;
*                AGE_GE7 = (TTDEATH1 >= 7)*AGE;
*                %mend;
*
*            %coxtvc(data = SURV,
*                    y = (TTDEATH0, TTDEATH1)*DEATH(0),
*                    x = GENDER AGE AGE_GE7,
*                    tvvar = AGE,
*                    nontvvar = GENDER,
*                    covs = COVS,
*                    ests = ESTS);
*     where COVS and ESTS are created by
*           data COVS;
*               input AGE GENDER;
*               datalines;
*               20 0
*               21 0
*               ;
*               run;
*
*           proc phreg data = SURV outest = ESTS;
*               model (TTDEATH0, TTDEATH1)*DEATH(0) = GENDER AGE AGE_GE7;
*               run;
*
*     This example highlights that various parameterizations can be used to
*     account for the time-varying coefficients.
*
*
*   Example 3: Multiple Violations, Fit Inside Macro
*     Consider the data described in Example 1
*            SUBJ   TTDEATH0  TTDEATH1  DEATH  AGE  GENDER
*            1      0          1        1      21   0
*            2      0          1        0      25   1
*            2      4          7        0      25   1
*            3      0          1        0      20   0
*            3      4          7        1      20   0
*            4      0          1        0      21   1
*            4      4          7        0      21   1
*            4      7         10        1      21   1
*     Notice that the variables used to account for the time-varying
*     coefficients are not present in the dataset. In this example, we will
*     use the macro to add these variables, fit the model, and obtain the
*     estimates of survival.
*
*     We consider that AGE has one effect prior to time 7 and another effect
*     effect at and following time 7. Suppose in addition we observe that
*     the GENDER effect changes over time in a log-linear fashion. We can
*     use the following code to fit this model and obtain survival estimates
*     for females aged 20 and 21.
*            data COVS;
*                input AGE GENDER;
*                datalines;
*                20 0
*                21 0
*                ;
*                run;
*
*            %macro vardefn;
*                if(TTDEATH1 < 7) then do;
*                    AGE_LT7 = AGE;
*                    AGE_GE7 = 0;
*                    end;
*
*                if(TTDEATH1 >= 7) then do;
*                    AGE_LT7 = 0;
*                    AGE_GE7 = AGE;
*                    end;
*
*                GENDER_LOGT = GENDER * log(TTDEATH1 + 0.00001);
*                %mend;
*
*            %coxtvc(data = SURV,
*                    y = (TTDEATH0, TTDEATH1) * DEATH(0),
*                    x = GENDER GENDER_LOGT AGE_LT7 AGE_GE7,
*                    tvvar = GENDER AGE,
*                    nontvvar = ,
*                    covs = COVS);
*
*     Since ESTS parameter is left empty, ESTS is created internally by
*     fitting the corresponding model. The variables used to account for the
*     time-varying coefficients (AGE_LT7 AGE_GE7 GENDER_LOGT) are added to
*     the SURV dataset prior to fitting the model.
*
*
*   Example 4: Survival Curves for Entire Population
*     Examples 1-3 dealth with obtaining survival estimates for a limited
*     number of patients. If we want to obtain survival estimates for the
*     entire population, it is possible by giving COVS the entire SURV
*     dataset. Some things should be considered. COVS should be limited
*     to variables found in the X, TVVAR, or NONTVVAR list. And, as SURV is
*     in the counting-process style of input, there are several duplicate
*     covariate records. We can quickly eliminate them using:
*            data COVS;
*                set SURV;
*                where TTDEATH0 = 1;
*                keep AGE GENDER;
*                run;
*
*
*   Example 5: Using Additional Statements
*     Consider Example 1. Suppose we want to run the model using GENDER as a
*     class variable instead of an indicator. We can do that using the
*     following:
*           data COVS;
*               input AGE GENDER;
*               datalines;
*               20 0
*               21 0
*               ;
*               run;
*
*            %macro vardefn;
*                AGE_LT7 = (TTDEATH1 < 7) * AGE;
*                AGE_GE7 = (TTDEATH1 >= 7) * AGE;
*                %mend;
*
*            %coxtvc(data = SURV,
*                    y = (TTDEATH0, TTDEATH1)*DEATH(0),
*                    x = GENDER AGE_LT7 AGE_GE7,
*                    tvvar = AGE,
*                    nontvvar = GENDER,
*                    covs = COVS,
*                    addstmts = %str(class GENDER;));
******************************************************************************/

%macro coxtvc(data = , y = , x = , tvvar = , nontvvar = , covs = , ests = ,
    modopts = , procopts = , addstmts = , out = SurvEsts);

    **************************************************************************
    * Fit the model, if applicable;
    %if (&ESTS = ) %then %do;
        *Run model and output estimates;
        proc phreg data = &DATA &PROCOPTS outest = _ests_;
            &ADDSTMTS.;
            model &Y = &X / &MODOPTS.;

            *Add time-varying variables in case not previously defined;
            %VARDEFN;
            run;

        %local ESTS;
        %let ESTS = _ests_;
        %end;



    **************************************************************************
    * Create COVS dataset
    *   1. Variables of type (B) and (C) will have the values specified by the
    *        user in COVS, if applicable.
    *   2. If not specified, variables of type (B) and (C) will take on their
    *        mean or reference value, if applicable. This is done using
    *        PHREG to get the same effect as BASELINE statement. To get
    *        correct averages, we use one-record-per-patient. In addition, as
    *        this procedure is only being used to obtain averages, the time-
    *        varying variables are not needed in the model at this point. They
    *        will be included in the actual modeling. ;

    *Determine minimum value of start time;
    proc means data = &DATA noprint;
        var %scan(&Y, 1, "()*,");
        output out = _means_ min = min;
        run;

    %local TMMIN;
    data _null_;
        set _means_;
        call symput("TMMIN", trim(left(min)));
        run;

    *Obtain correct value for time-varying covariates when not specified;
    proc phreg data = &DATA noprint;
        *only considering one-record-per-patient ensures correct averages;
        where %scan(&Y, 1, "()*,") = &TMMIN;
        &ADDSTMTS.;
        model &Y = &TVVAR &NONTVVAR;

        *obtain correct values;
        baseline out = _means_;
        run;

    *Remove death-time from dataset and keep only one record;
    data _means_;
        set _means_;
        keep &TVVAR &NONTVVAR;
        if (_n_ = 1);
        run;

    *If COVS pre-specified, set all missing variables to default values
    *  obtained from PHREG above;
    %if (&COVS ^= ) %then %do;
        data &COVS.;
            if(_n_ = 1) then set _means_;
            set &COVS;
            run;
        %end;

    *If COVS not pre-specified, set all variables to default value;
    %if (&COVS = ) %then %do;
        %let COVS = _means_;
        %end;

    *Add time-varying variables to COVS dataset;
    %local I NTVVAR;
    data _temp_covs_;
        %let I = 1;
        %do %while(%scan(&X, &I, %str( )) ^= );
            %scan(&X, &I, %str( )) = 0;
            %let I = %eval(&I + 1);
            %end;

        %let I = 1;
        %do %while(%scan(&TVVAR, &I, %str( )) ^= );
            %scan(&TVVAR, &I, %str( )) = 0;
            %let NTVVAR = %eval(&I);
            %let I = %eval(&I + 1);
            %end;
        run;

    data &COVS;
        *Set only variables accounting for time-varying coefficients;
        if (_n_ = 1) then set _temp_covs_(drop = &TVVAR &NONTVVAR);
        set &COVS;
        run;



    **************************************************************************
    * Create dataset that contains the unique records of TVVAR variables;

    *Obtain unique records;
    proc sort data = &COVS (keep = &TVVAR) out = _unique_ nodupkey;
        by &TVVAR;
        run;

    *Determine the number of unique records;
    %local NUNIQUE;
    data _null_;
        set _unique_;
        call symput("NUNIQUE", trim(left(_n_)));
        run;

    *Sort COVS dataset for later use;
    proc sort data = &COVS out = &COVS;
        by &TVVAR;
        run;



    **************************************************************************
    * Iterate through unique records;

    *Define macros to hold value of interest;
    %do I = 1 %to &NTVVAR;
        %local XTV&I;
        %end;

    %local J;
    %do I = 1 %to &NUNIQUE;
        *Assign to macro variables the unique values for this record;
        data _null_;
            _point_ = &I;
            *grab only record of interest;
            set _unique_ point = _point_;

            %do J = 1 %to &NTVVAR;
                call symput("XTV&J", trim(left(%scan(&TVVAR, &J, %str( )))));
                %end;
            stop;
            run;

        *Restructure time-varying variables to perform integration;
        data &DATA._temp;
            set &DATA.;

            *Reparameterize variables with time-varying coefficients;
            %do J = 1 %to &NTVVAR;
                %scan(&TVVAR, &J, %str( )) =
                  %scan(&TVVAR, &J, %str( )) - &&XTV&J;
                %end;

            %VARDEFN;
            run;

        *Put other variables back to normal;
        data &DATA._temp;
            set &DATA._temp;
            set &DATA.(keep = &TVVAR);
            run;

        *Get only relevant records from COVS dataset;
        data &COVS._temp;
            set &COVS.;
            by &TVVAR;

            retain _point_ 0;
            _any_first_ = 0;
            %do J = 1 %to &NTVVAR;
                _any_first_ =
                  max(_any_first_, first.%scan(&TVVAR, &J, %str( )));
                %end;

            if (_any_first_ = 1) then _point_ = _point_ + 1;

            *keep only relevant records;
            if(_point_ = &I) then output;

            *only keep variables in the model;
            keep &X;
            run;

        *Get survival estimates;
        proc phreg data = &DATA._temp inest = &ESTS. noprint;
            &ADDSTMTS.;
            model &Y. = &X / maxiter = 0;

            baseline out = &OUT._temp covariates = &COVS._temp
              survival = Shat / method = emp;
            run;

        *Update output dataset;
        %if (&I = 1) %then %do;
            data &OUT;
                set &OUT._temp;
                run;
            %end;

        %if (&I ^= 1) %then %do;
            data &OUT;
                set &OUT
                    &OUT._temp;
                run;
            %end;
        %end;

    %exit:
    %mend coxtvc;
