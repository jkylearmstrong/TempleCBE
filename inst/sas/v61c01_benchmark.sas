
*--------------------------------- SAS CODE from Manuscript -------------------------------------------;

* Make the toy data set referenced in the paper;

data SURV; 
input id time death age female;
datalines;
1 1 1 20 0
2 4 0 21 1
3 7 1 19 0
4 10 1 22 1
5 12 0 20 0
6 13 1 24 1
;
run;


* Convert SURV into the counting process style;

%let FILEPATH = C:\ ;
%include "&FILEPATH.cpdata.sas";

%cpdata(data = SURV, time = time, event = death(0), outdata = SURV2);
proc print data = SURV2; run;


* Basic Cox proportional hazards model;

proc phreg data = SURV;
    class female (ref = "0");
    model time*death(0) = female age;
    baseline out = outset survival = survival / method = emp;
    run;


* Time-varying coefficient model;

proc phreg data = SURV;
    class female (ref = "0");
    model time*death(0) = female age lt_age;
    lt_age = age * log(time);
    run;


* Alternative specification of the time-varying coefficient model with failed attempt to estimate survival;

data SURV2;
    set SURV2;
    lt_age = age * log(time1);
    run;

proc phreg data = SURV2;
    class female (ref = "0");
    model (time0, time1)*death(0) = female age lt_age;
    baseline out = outset survival = survival / method = emp;
    run;


* Prepare correct covariates data set for survival estimation with time-varying coefficient model;

data  SURV3;
    set SURV2;
    age = age - 21;
    lt_age = age * log(time1);
    run;

data covs;
    age = 0;
    lt_age = 0;
    female = 0;
    run;


* Survival estimation for time-varying coefficient model;

proc phreg data = SURV3;
    class female (ref = "0");
    model (time0, time1)*death(0) = age lt_age female;
    baseline out = outset survival = survival covariates = covs /
        method = emp;
    run;
proc print data = outset; run;


* Using the coxtvc macro to perform the same survival estimation;

data covs;
    age = 21;
    female = 0;
    run;

%macro vardefn;
    lt_age = age * log(time1);
    %mend;

%include "&FILEPATH.coxtvc.sas";
%coxtvc(data = SURV2,
    y = (time0, time1)*death(0),
    x = age lt_age female,
    tvvar = age,
    nontvvar = female,
    covs = covs);


* Extended use of the same macro;

%macro vardefn;
    if (time1 < 7) then do;
        female_lt7 = female;
        female_ge7 = 0;
        end;
        
    if (time1 >= 7) then do;
        female_lt7 = 0;
        female_ge7 = female;
        end;

    lt_age = age * log(time1);
    %mend;

%coxtvc(data = SURV2, 
    y = (time0, time1)*death(0),
    x = female_lt7 female_ge7 age lt_age,
    tvvar = female age, 
    nontvvar = , 
    covs = covs);


*--------------------------------- Rossi data analysis example ------------------------------------;

libname proj "&FILEPATH.";   * FILEPATH is a macro variable defined previously;


* Fit time-varying coefficient model;

data rossi; set proj.recid;
    keep week arrest fin age race wexp mar paro prio educ;
    run;
    
proc phreg data = rossi;
    model week*arrest(0) = age fin prio age_week fin_mid / rl;
    age_week = age * week;
    fin_mid = fin * (20 < week < 30);
    run;


* Estimate adjusted event probabilities from the time-varying coefficient model, according to financial aid status; 

data covs;
    set rossi (keep = age prio);
    do fin = 0 to 1;
        output;
        end;
    run;
    
%cpdata(data = rossi,
        time = week,
        event = arrest(0),
        outdata = rossi2);
        
%macro vardefn;
    age_week = age * week1;
    fin_mid = fin * (20 < week1 < 30);
    %mend vardefn;
        
%coxtvc(data = rossi2,
        y = (week0, week1)*arrest(0),
        x = age fin prio age_week fin_mid,
        tvvar = age fin,
        nontvvar = prio,
        covs = covs,
        out = survest);
        
proc sort data = survest;
    by fin week1;
    run;
    
data avgsurv;
    set survest;
    by fin week1;
    
    retain sumshat total;
    if (first.week1) then do;
        sumshat = 0;
        total = 0;
        end;
        
    sumshat = sumshat + shat;
    total = total + 1;
    
    if (last.week1) then do;
        avgshat = sumshat / total;
        output;
        end;
    run;


* ----------------------------------- SAS Code from APPENDIX ------------------------------------------------------;

/******************************************************************************
* Macro: cpdata
* Description: Creates a dataset for survival analysis in the counting process
*              style of input from a one-record-per-patient survival dataset.
*
* Parameters:
*   DATA       Input dataset containing all variables of interest.
*   TIME       Survival time.
*   EVENT      Event indicator. Variable used to indicate if there is an
*                event. This should be followed by the list of values that
*                EVENT takes on if it is censored. See PHREG for details.
*                If no censoring took place, include a variable in DATA that
*                takes only value 1. Then, the value inside the parenthesis
*                is the value you want the censoring indicator to take when
*                censoring occurs.
*   OUTDATA    Output dataset.
*
* Usage:
*   Consider the following SAS dataset:
*        data temp;
*            input survtime death age treatment;
*            datalines;
*            1  1 20 0
*            4  0 21 1
*            7  1 19 0
*            10 1 25 0
*            ;
*            run;
*
*   We want to transform this dataset into the following:
*            survtime0 survtime1 death age treatment
*            0         1         1     20  0
*            0         1         0     21  1
*            0         1         0     19  0
*            1         7         1     19  0
*            0         1         0     25  0
*            1         7         0     25  0
*            7         10        1     25  0
*
*   We note that intervals are calculated only for times at which an event
*   occurs. This speeds up computation, and the results are identical to
*   those when intervals are calculated for all times (event and censored).
*
*   To create this dataset, we simply run the following
*     %cpdata(DATA = temp, TIME = survtime, EVENT = death(0), OUTDATA = out);
*
*   The resulting dataset is stored in the SAS dataset OUT.
******************************************************************************/

%macro cpdata(DATA = , TIME = , EVENT = , OUTDATA = );
    * Separate event variable from list of censoring;
    %local CLIST;
    %let CLIST = %scan(&EVENT., 2, "()");
    %let EVENT = %scan(&EVENT., 1, "()");

    * Remove non-unique event times;
    proc sort data = &DATA. (where = (&EVENT. ^in (&CLIST.)))
        out = &OUTDATA. (keep = &TIME.) nodupkey;
        by &TIME.;
        run;

    * Create a list of all time intervals;
    * Only include intervals where an event occurs;
    data &OUTDATA.;
        set &OUTDATA.(rename = (&TIME. = &TIME.1));

        drop temp;
        retain temp 0;

        &TIME.0 = temp;
        temp = &TIME.1;
        run;

    * Expand current dataset;
    %let CLIST = %scan(&CLIST.,1);
    data &OUTDATA.;
        set &DATA.;

        _temp_ = &EVENT.;
        
        drop &TIME. _temp_;
        do _point_ = 1 to _nobs_;
            set &OUTDATA. point = _point_ nobs = _nobs_;

            *if current time beyond subjects event time, do not read;
            if(&TIME. < &TIME.1) then leave;

            *determine if current time is the subjects event time;
            if(&TIME. ^= &TIME.1) then &EVENT. = &CLIST.;
            if(&TIME. = &TIME.1) then &EVENT. = _temp_;
            output;
            end;
        run;

    %mend cpdata; 

 
 
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


*----------------------------------- SAS code for simulation study-----------------------------------;

%let FILEPATH = C:\ ;

%include "&FILEPATH.cpdata.sas";
%include "&FILEPATH.coxtvc.sas";

libname proj "&FILEPATH.";


*******************************************************************************
* macro: genDat
* purpose: Generate data that follows an proportional hazards model with a
*          time-varying coefficient.
*
* model: The underlying survival times follow an exponential distribution with
*        a time-varying coefficient:
*               S(t) = exp{-(t / lambda) * exp(xb1)} * I(t <= A) +
*                   exp{-(A / lambda) * exp(xb1) -
*                   ((t - A) / lambda) * exp(xb2)} * I(t > A)
*                 where x is in {0,1}.
*
*        A is determined by taking half of the maximum censoring time.
*        Censoring times are uniformly distributed between [0, CMAX] and are
*        independent of all other variables.
*
* parameters:
*   n         Sample size.
*   beta1     Log hazard ratio for first time interval.
*   beta2     Log hazard ratio for second time interval.
*   lambda    Mean survival time under no treatment. Keep in mind this does not
*               include any affect due to censoring.
*   cmax      Maximum censoring time permitted.
*   m         Number of replicates to generate.
*   outdata   SAS dataset in which to store results. ;

%macro genDat(n = , beta1 = , beta2 = , lambda = , cmax = , m = , outdat = ,
              seed = 110310);

    data &OUTDAT;
        *time to events stored in y1 - yM;
        array y{&M.};
        *event indicators stored in event1 - eventM;
        array event{&M.};
        *treatment indicatores stored in trt1 - trtM;
        array trt{&M.};

        *set seed for random number generation;
        call streaminit(&SEED);

        *for each observation, generate all replicates;
        do i = 1 to &N;
            do j = 1 to &M;
                *trt is binary, with probability 0.5;
                trt[j] = rand("binomial", 0.5, 1);
                *survival times follow exponential dist with mean = 1 / lambda;
                hazard1 = exp(&BETA1. * trt[j]) / &LAMBDA.;
                hazard2 = exp(&BETA2. * trt[j]) / &LAMBDA.;

                eventtm1 = rand("exponential") / hazard1;
                eventtm2 = rand("exponential") / hazard2;

                *break occurs at 0.5 * CMAX;
                eventtm = eventtm1 * (eventtm1 <= (0.5 * &CMAX)) +
                    (eventtm2 + (0.5 * &CMAX)) * (eventtm1 > (0.5 * &CMAX));

                *censoring times;
                censtm = rand("uniform") * &CMAX;

                *event times and indicators;
                y[j] = min(eventtm, censtm);
                event[j] = (eventtm < censtm);
                end;
            output;
            end;

        drop i j hazard1 hazard2 eventtm1 eventtm2 eventtm censtm;
        run;
    %mend genDat;



%macro simEsts(data = , m = , cut = , times = , outdat = );
    %do Z = 1 %to &M;
        *keep only current dataset;
        data _temp_;
            set &DATA.;
            y = y&Z.;
            trt = trt&Z.;
            event = event&Z.;
            keep y trt event;
            run;

        *expand data;
        %cpdata(data = _temp_, 
                time = y,
                event = event(0),
                outdata = _tempdat_);

        *add time dependent covariate;
        data _tempdat_;
            set _tempdat_;

            trtA = (y1 <= &CUT) * trt;
            trtB = (y1 > &CUT) * trt;
            run;

        *set up covariates;
        data covs;
            trt = 1;
            run;

        *obtain survival estimates;
        %macro vardefn;
            trtA = (y1 <= &CUT) * trt;
            trtB = (y1 > &CUT) * trt;
            %mend vardefn;

        %coxtvc(data = _tempdat_,
                y = (y0, y1)*event(0),
                x = trtA trtB,
                tvvar = trt,
                nontvvar = ,
                covs = covs,
                procopts = %str(noprint),
                out = _SurvEsts_);

        data _SurvEsts_;
            set _SurvEsts_;

            keep y1 shat;
            run;

        *obtain estimates at specified times;
        data _tempdat_;
            do y1 = &TIMES.;
                output;
                end;
            run;

        data _SurvEsts_;
            merge _tempdat_(in = in__tempdat_)
                  _SurvEsts_(rename = (shat = temp));
            by y1;

            retain shat;

            *determine correct survival estimate;
            if(temp ^= .) then shat = temp;

            if(in__tempdat_);

            keep y1 shat;
            run;

        *add to output;
        data &OUTDAT;
            %if(&Z > 1) %then %do; set &OUTDAT; %end;
            set _SurvEsts_(rename = (shat = shat&Z));
            run; 

        %end;
     %mend simEsts;




%let simreps = 500;

%genDat(n = 2500,
        beta1 = -0.8,
        beta2 = 0.8,
        lambda = 8,
        cmax = 8,
        m = &simreps,
        outdat = simDat,
        seed = 110310);

data proj.simDat;
    set simDat;
    run;

%simEsts(data = simDat,
         m = &simreps,
         cut = 4,
         times = 0 to 8 by 0.1,
         outdat = survHat);

data proj.survHat;
    set survHat;

    *Add true survival;
    shat0 = exp(-(1/8) * (min(y1, 4) * exp(-0.8) + 
      (max(y1, 4) - 4) * exp(0.8)));
    run;


*** Write CSV file with results;
%macro writem;
    %do I = 1 %to &simreps;
        shat&I.
        %end;
    %mend writem;

data _null_;
    set proj.survhat;

    file "&FILEPATH.rdata.csv" dlm = "," dsd linesize = 6500;

    put y1 shat0 %writem;;
    run;



*----------------------------------- Rossi data analysis -----------------------------------; 
* Same as in main text except for details of data output for plotting;


libname proj "&FILEPATH.";

data rossi; set proj.recid;
    keep week arrest fin age race wexp mar paro prio educ;
    run;
    
data covs;
    set rossi (keep = age prio);
    do fin = 0 to 1;
        output;
        end;
    run;
    
%cpdata(data = rossi,
        time = week,
        event = arrest(0),
        outdata = rossi2);
        
%macro vardefn;
    age_week = age * week1;
    fin_mid = fin * (20 < week1 < 30);
    %mend vardefn;
        
%coxtvc(data = rossi2,
        y = (week0, week1)*arrest(0),
        x = age fin prio age_week fin_mid,
        tvvar = age fin,
        nontvvar = prio,
        covs = covs,
        out = survest);
        
proc sort data = survest;
    by fin week1;
    run;
    
data survest;
    set survest;
    by fin week1;
    
    retain sumshat total;
    if (first.week1) then do;
        sumshat = 0;
        total = 0;
        end;
        
    sumshat = sumshat + shat;
    total = total + 1;
    
    if (last.week1) then do;
        avgshat = sumshat / total;
        output;
        end;
    run;
    
proc phreg data = rossi noprint;
    model week*arrest(0) = age fin prio;
    baseline out = phests covariates = covs survival = shat / method = emp;
    run;
    
proc sort data = phests;
    by fin week;
    run;
    
data phests;
    set phests;
    by fin week;
    
    retain sumshat total;
    if (first.week) then do;
        sumshat = 0;
        total = 0;
        end;
        
    sumshat = sumshat + shat;
    total = total + 1;
    
    if (last.week) then do;
        avgshat = sumshat / total;
        output;
        end;
    run;
    
data avgsurv;
    merge survest(rename = (week1 = week avgshat = avgtvests))
          phests(rename = (avgshat = avgphests));
    by fin week;
    run;

data _null_;
    set avgsurv;

    file "&FILEPATH.avgsurv.csv" dlm = "," dsd linesize = 6500;

    if(_n_ = 1) then put "week,age,fin,prio,avgphests,avgtvests";
    put week age fin prio avgphests avgtvests;
    run;


