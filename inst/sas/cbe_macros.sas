/*==============================================================================
  TempleCBE SAS Macro Library: Master Loader
  Package: TempleCBE (R & SAS Biostatistics Survival Framework)
  Authors: J. Kyle Armstrong & TempleCBE Team
  
  Usage in SAS:
    %let templecbe_sas = <path-to-TempleCBE/inst/sas>;
    %include "&templecbe_sas/cbe_macros.sas";
    
  Or in R:
    sas_macro_file <- TempleCBE::cbe_sas_macro_path("cbe_macros.sas")
==============================================================================*/

%put NOTE: ====================================================================;
%put NOTE: Loading TempleCBE SAS Macro Suite (v0.2.1);
%put NOTE: Provides clinical biostatistics tools for parity with TempleCBE (R):;
%put NOTE:   1. %nrstr(%cbe_brier_score)      - IPCW Graf Brier Score & IBS Calibration;
%put NOTE:   2. %nrstr(%cbe_cox_phreg)        - Standardized PROC PHREG & Diagnostics;
%put NOTE:   3. %nrstr(%cbe_counting_process) - Repeated Measures to Counting Process;
%put NOTE:   4. %nrstr(%coxtvc)               - Survival Estimation with Time-Varying Coefs;
%put NOTE:   5. %nrstr(%cpdata)               - Counting Process Data Splitting;
%put NOTE: ====================================================================;

/* Automatically resolve the directory of this macro file if executed via %include */
%macro _cbe_resolve_dir;
    %global _CBE_MACRO_DIR;
    %if %symexist(templecbe_sas) %then %do;
        %let _CBE_MACRO_DIR = &templecbe_sas;
    %end;
    %else %do;
        %let _CBE_MACRO_DIR = %sysfunc(pathname(work));
    %end;
%mend _cbe_resolve_dir;
%_cbe_resolve_dir;

/* Load Individual Macro Modules */
%if %sysfunc(fileexist(&_CBE_MACRO_DIR/cbe_brier_score.sas)) %then %do;
    %include "&_CBE_MACRO_DIR/cbe_brier_score.sas";
%end;

%if %sysfunc(fileexist(&_CBE_MACRO_DIR/cbe_cox_phreg.sas)) %then %do;
    %include "&_CBE_MACRO_DIR/cbe_cox_phreg.sas";
%end;

%if %sysfunc(fileexist(&_CBE_MACRO_DIR/cbe_counting_process.sas)) %then %do;
    %include "&_CBE_MACRO_DIR/cbe_counting_process.sas";
%end;

%if %sysfunc(fileexist(&_CBE_MACRO_DIR/coxtvc.sas)) %then %do;
    %include "&_CBE_MACRO_DIR/coxtvc.sas";
%end;

%if %sysfunc(fileexist(&_CBE_MACRO_DIR/cpdata.sas)) %then %do;
    %include "&_CBE_MACRO_DIR/cpdata.sas";
%end;

%put NOTE: TempleCBE SAS Macro Suite successfully initialized.;
