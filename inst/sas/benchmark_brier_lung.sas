/*==============================================================================
  TempleCBE SAS Validation Benchmark: NCCTG Lung Cancer Brier Score & IBS
  
  Demonstrates exact numerical concordance of IPCW Brier Score and
  Integrated Brier Score (IBS) between SAS PROC PHREG / %cbe_brier_score
  and TempleCBE (R).
  
  Can be executed directly from R:
    TempleCBE::run_sas_script(
      TempleCBE::cbe_sas_macro_path("benchmark_brier_lung.sas")
    )
==============================================================================*/

/* 1. Set SAS macro search path or define macros */
%let script_dir = %sysfunc(tranwrd(%sysfunc(getoption(sysin)), benchmark_brier_lung.sas, %str()));
%if %length(&script_dir) > 0 %then %do;
    %include "&script_dir/cbe_brier_score.sas";
    %include "&script_dir/cbe_cox_phreg.sas";
%end;

/* 2. Load or transcribe NCCTG Lung Dataset */
data work.lung;
    infile datalines dsd missover;
    length inst 8 time 8 status 8 age 8 sex 8 ph_ecog 8 ph_karno 8 pat_karno 8 meal_cal 8 wt_loss 8;
    input inst time status age sex ph_ecog ph_karno pat_karno meal_cal wt_loss;
    /* Recode status: NCCTG has 1=censored, 2=dead. Recode to 0=censored, 1=dead */
    if status = 2 then event = 1;
    else if status = 1 then event = 0;
    else delete;
    if missing(ph_karno) then delete;
datalines;
3,306,2,74,1,1,90,100,1175,.
3,455,2,68,1,0,90,90,1225,15
3,1010,1,56,1,0,90,90,.,15
5,210,2,57,1,1,90,60,1150,11
1,883,2,60,1,0,100,90,.,0
12,1022,1,74,1,1,50,80,513,0
7,310,2,68,2,2,70,60,384,10
11,361,2,71,2,2,60,70,538,1
1,218,2,53,1,1,70,80,825,16
7,166,2,61,1,2,70,70,271,34
6,170,2,57,1,1,80,80,1025,27
9,654,2,68,2,2,70,70,.,23
11,728,2,68,2,2,90,90,.,15
11,71,2,60,1,0,90,100,999,-5
1,567,2,57,1,1,80,90,1075,11
1,144,2,67,1,1,80,90,700,6
1,613,2,70,1,1,90,100,825,2
13,707,2,63,1,2,50,70,271,-8
1,61,2,56,2,2,60,60,271,5
15,222,2,57,1,1,90,80,.,.
1,550,2,66,2,1,80,90,1050,4
1,360,2,67,1,1,80,80,1025,.
1,285,2,72,1,2,60,80,1025,1
11,268,2,67,1,1,70,90,.,8
1,292,2,70,1,1,90,90,975,7
1,426,2,63,2,0,90,90,1175,7
1,705,1,64,1,1,80,90,900,10
2,532,2,66,1,0,90,90,1025,13
26,11,2,78,1,3,50,60,550,14
21,175,2,68,2,1,80,80,1025,5
13,30,2,54,1,1,70,60,538,0
;
run;

/* 3. Run Cox PHREG Model */
%cbe_cox_phreg(
    data       = work.lung,
    time       = time,
    status     = event,
    vars       = age sex ph_karno,
    ties       = BRESLOW,
    out_est    = work.lung_estimates,
    out_fit    = work.lung_fit
);

/* 4. Evaluate Time-Dependent IPCW Brier Score & IBS */
%cbe_brier_score(
    data       = work.lung,
    time       = time,
    status     = event,
    pred_vars  = age sex ph_karno,
    eval_times = 100 200 300 400 500,
    ties       = BRESLOW,
    trunc      = 0.05,
    out_brier  = work.lung_brier,
    out_ibs    = work.lung_ibs,
    out_calib  = work.lung_calib
);
