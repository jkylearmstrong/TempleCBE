/*==============================================================================
  TempleCBE SAS Validation Benchmark: PROC COMPARE against cbe_compare_df()

  Nine small comparisons, each on data typed into this program (so R and SAS see
  the very same numbers; a test in test-sas_parity_reference.R checks the
  datalines). All use METHOD=ABSOLUTE and a CRITERION that is the tolerance given
  to cbe_compare_df(): 1E-7 (its default), except s4_edge (0.25) and s9_trap (0.1).

    s1_id       ID id. Identical variables, a difference inside and beyond the
                criterion, a missing value on one side only, a missing value on
                both sides, character differences (including case and a blank),
                a variable that is numeric in BASE and character in COMPARE, a
                variable only in BASE and one only in COMPARE, a character
                variable with different lengths, two observations only in BASE
                and one only in COMPARE. COMPARE is typed unsorted and sorted
                here, because PROC COMPARE needs sorted data; cbe_compare_df()
                does not.
    s2_rows     No ID: observations are compared by position; BASE has two more.
    s3_dup      ID values that occur more than once (paired in order).
    s4_edge     Differences of exactly the criterion, just below and just above.
    s5_decimal  Differences of 9E-8 and 1.1E-7 at criterion 1E-7, on numbers of
                different size and in both directions.
    s6_same     A data set compared with a copy of itself.
    s7_within   Differences that are all inside the criterion.
    s8_text     Character differences: case, and a trailing blank.
    s9_trap     A difference of 0.1 (as a decimal) at criterion 0.1: decimal
                fractions are not exact in binary, so it is a difference for
                1 against 1.1 and none for 0.5 against 0.6.

  The listing is read by tests/testthat/helper-sas-reference.R (parse_compare_lst);
  the numbers are in tests/testthat/reference/sas_proc_compare.csv. SYSINFO is the
  return code PROC COMPARE leaves in &SYSINFO (a bit mask), saved before the DATA
  step that prints it, because a DATA step resets it.

  Can be executed directly from R:
    TempleCBE::run_sas_script(
      TempleCBE::cbe_sas_macro_path("benchmark_proc_compare.sas")
    )
==============================================================================*/

options nodate nonumber linesize=100 pagesize=32767;

%macro show_sysinfo(scenario);
    %let rc = &sysinfo;
    data _null_;
        file print;
        put "SYSINFO &scenario &rc";
    run;
%mend show_sysinfo;

/*------------------------------------------------------------------------------
  s1_id
------------------------------------------------------------------------------*/
data work.s1_base;
    infile datalines dsd missover;
    length id same_num within_tol over_tol miss_num conf only_base 8
           chr_diff $ 8 chr_blank $ 8 chr_len $ 8;
    input id same_num within_tol over_tol miss_num chr_diff $ chr_blank $ chr_len $ conf only_base;
datalines;
1,10,1.5,5,1.1,abc,x,alpha,1,100
2,20,2.5,6,2.2,abc,y,beta,2,200
3,30,3.5,7,3.3,ghi,z,gamma,3,300
4,40,4.5,8,.,jkl,w,delta,4,400
5,50,5.5,9,5.5,mno,,epsilon,5,500
6,60,6.5,10,6.6,pqr,v,zeta,6,600
7,70,7.5,11,7.7,stu,u,eta,7,700
8,80,8.5,12,8.8,vwx,y,theta,8,800
9,90,9.5,13,.,yza,,iota,9,900
10,100,10.5,14,10.1,bcd,t,kappa,10,1000
;
run;

data work.s1_comp;
    infile datalines dsd missover;
    length id same_num within_tol over_tol miss_num 8 only_comp $ 8
           chr_diff $ 8 chr_blank $ 8 chr_len $ 12 conf $ 8;
    input id same_num within_tol over_tol miss_num chr_diff $ chr_blank $ chr_len $ conf $ only_comp $;
datalines;
11,110,11.5,15,11.1,efg,s,lambda,11,k
5,50,5.5,11.5,5.5,mno,x,epsilon,5,e
1,10,1.50000005,5,1.1,abc,x,alpha,1,a
3,30,3.5,7.001,3.3,ghi,z,gamma,3,c
2,20,2.5,6.00000008,2.2,abd,y,beta,2.0,b
4,40,4.5,8,4.4,jkl,w,delta,4,d
6,60,6.49999997,10,.,Pqr,v,zeta,6,f
8,80,8.5,12.000001,8.8,vwx,,theta,8,h
9,90,9.5,13,.,yza,,iota,9,i
;
run;

proc sort data=work.s1_comp;
    by id;
run;

title "Scenario s1_id: ID id, METHOD=ABSOLUTE CRITERION=1E-7";
proc compare base=work.s1_base compare=work.s1_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s1_id)

/*------------------------------------------------------------------------------
  s2_rows
------------------------------------------------------------------------------*/
data work.s2_base;
    infile datalines dsd missover;
    length a b c 8 s $ 8;
    input a b c s $;
datalines;
1,10,100,p
2,20,200,q
3,30,.,r
4,40,400,s
5,50,500,t
6,60,600,u
7,70,700,v
;
run;

data work.s2_comp;
    infile datalines dsd missover;
    length a b c 8 s $ 8;
    input a b c s $;
datalines;
1,10,100,p
2,21.5,200,q
3,30,300,x
4,40,400,s
5,50,500.0000002,t
;
run;

title "Scenario s2_rows: no ID, observations compared by position";
proc compare base=work.s2_base compare=work.s2_comp method=absolute criterion=1e-7 listall listequalvar;
run;
%show_sysinfo(s2_rows)

/*------------------------------------------------------------------------------
  s3_dup
------------------------------------------------------------------------------*/
data work.s3_base;
    infile datalines dsd missover;
    length id v 8;
    input id v;
datalines;
1,10
1,11
2,20
3,30
3,31
3,32
;
run;

data work.s3_comp;
    infile datalines dsd missover;
    length id v 8;
    input id v;
datalines;
1,10
1,11
1,12
2,20
3,30
3,99
;
run;

title "Scenario s3_dup: ID values that occur more than once";
proc compare base=work.s3_base compare=work.s3_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s3_dup)

/*------------------------------------------------------------------------------
  s4_edge: CRITERION=0.25 (exact in binary)
------------------------------------------------------------------------------*/
data work.s4_base;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1
2,1
3,1
4,1
5,1
6,1
7,1
;
run;

data work.s4_comp;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1.25
2,1.2500001
3,1.2499999
4,0.75
5,1.5
6,0.7499999
7,1.250000000000002
;
run;

title "Scenario s4_edge: ID id, METHOD=ABSOLUTE CRITERION=0.25";
proc compare base=work.s4_base compare=work.s4_comp method=absolute criterion=0.25 listall listequalvar;
    id id;
run;
%show_sysinfo(s4_edge)

/*------------------------------------------------------------------------------
  s5_decimal
------------------------------------------------------------------------------*/
data work.s5_base;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1
2,1
3,100
4,100
5,0.001
6,0.001
7,1
8,1
;
run;

data work.s5_comp;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1.00000009
2,1.00000011
3,100.00000009
4,100.00000011
5,0.00100009
6,0.00100011
7,0.99999991
8,0.99999989
;
run;

title "Scenario s5_decimal: ID id, METHOD=ABSOLUTE CRITERION=1E-7";
proc compare base=work.s5_base compare=work.s5_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s5_decimal)

/*------------------------------------------------------------------------------
  s6_same
------------------------------------------------------------------------------*/
data work.s6_comp;
    set work.s1_base;
run;

title "Scenario s6_same: ID id, a data set and a copy of it";
proc compare base=work.s1_base compare=work.s6_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s6_same)

/*------------------------------------------------------------------------------
  s7_within
------------------------------------------------------------------------------*/
data work.s7_base;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1
2,100
3,0.1
4,5
;
run;

data work.s7_comp;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1.00000005
2,100.00000005
3,0.10000005
4,5.00000005
;
run;

title "Scenario s7_within: ID id, every difference inside the criterion";
proc compare base=work.s7_base compare=work.s7_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s7_within)

/*------------------------------------------------------------------------------
  s8_text
------------------------------------------------------------------------------*/
data work.s8_base;
    infile datalines dsd missover;
    length id n 8 s $ 8;
    input id s $ n;
datalines;
1,abc,1
2,def,2
3,ghi,3
4,jkl,4
;
run;

data work.s8_comp;
    infile datalines dsd missover;
    length id n 8 s $ 8;
    input id s $ n;
datalines;
1,abc,1
2,def  ,2
3,GHI,3
4,jkl,4
;
run;

title "Scenario s8_text: ID id, character values";
proc compare base=work.s8_base compare=work.s8_comp method=absolute criterion=1e-7 listall listequalvar;
    id id;
run;
%show_sysinfo(s8_text)

/*------------------------------------------------------------------------------
  s9_trap: CRITERION=0.1 (not exact in binary)
------------------------------------------------------------------------------*/
data work.s9_base;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1
2,1
3,0.5
;
run;

data work.s9_comp;
    infile datalines dsd missover;
    length id x 8;
    input id x;
datalines;
1,1.1
2,1.09999
3,0.6
;
run;

title "Scenario s9_trap: ID id, METHOD=ABSOLUTE CRITERION=0.1";
proc compare base=work.s9_base compare=work.s9_comp method=absolute criterion=0.1 listall listequalvar;
    id id;
run;
%show_sysinfo(s9_trap)
