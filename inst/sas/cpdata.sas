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

