# Collapse Survival Outcomes to One Row per Subject

Turns right-censored or counting-process (start/stop) survival outcomes
into the one-row-per-subject, right-censored truth that yardstick's
survival metrics require.

## Usage

``` r
surv_subject_truth(
  truth,
  subject_id = NULL,
  recurrent = c("error", "first", "last")
)
```

## Arguments

- truth:

  A \[survival::Surv()\] object: \`Surv(time, event)\` or \`Surv(start,
  stop, event)\`.

- subject_id:

  Subject identifiers, one per element of \`truth\`. Required for
  counting-process data; for right-censored data each row is its own
  subject when \`subject_id\` is \`NULL\`.

- recurrent:

  \`"error"\` (default) stops when a subject has an event before their
  last interval; \`"first"\` keeps each subject's time to first event;
  \`"last"\` keeps each subject's last interval (end of follow-up), for
  estimating the calendar censoring distribution in gap-time scoring.

## Value

A tibble with one row per subject, sorted by \`subject_id\`: \*
\`.subject_id\`: the subject identifier, with the type and values of
\`subject_id\` (the row number, \`1:n\`, for right-censored data without
\`subject_id\`). Use it to join the truth back to subject-level
predictions or covariates; \[collapse_start_stop_predictions()\] and the
\`.predictions\` of \[tune_grid_start_stop()\] carry the same column. \*
\`.entry\`: the start of the subject's first interval (0 for
right-censored data). \* \`.truth\`: a right-censored \`Surv\` object:
the stop of the last kept interval and its event status.

## Details

For counting-process data, a subject's time is the stop time of their
last interval and their event status is the status of that interval.
Rows are checked first: within a subject, intervals must not overlap,
and an event may only occur in the last interval, unless \`recurrent =
"first"\` (time to first event) or \`recurrent = "last"\` (end of
follow-up, below).

With \`recurrent = "first"\` (recurrent events, such as repeated
readmissions) a subject is scored on the time to their \*\*first\*\*
event: the rows after the first event are ignored, and a subject without
an event is censored at the stop of their last interval. That is the
one-row-per-subject outcome that yardstick's survival metrics can score.
The recurrent process itself is modelled with \[recurrent_cox()\]; to
score the whole recurrent process instead of only the first event, see
\[surv_gap_truth()\] and \`cv_coxnet(recurrent = "gap")\`, which score
gap-time concordance.

With \`recurrent = "last"\` a subject is summarised by the \*\*end of
their follow-up\*\*: the stop of their last interval, with that
interval's status (an event in the last interval, or censoring). That is
the outcome the calendar censoring distribution is estimated from for
gap-time scoring (\`cv_coxnet(recurrent = "gap")\`): a subject's
censoring time \`C\` is only revealed when follow-up ends in censoring,
so first-event times would understate late censoring – most subjects are
still under follow-up long after their first event, and a reverse
Kaplan-Meier on first-event times is badly biased at late times. Do not
use \`recurrent = "last"\` to score a subject's survival: it describes
when follow-up ended, not when the subject's event happened.

## See also

\[censoring_km()\], \[graf_weights()\], \[add_graf_weights()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  long <- data.frame(
    subject_id = c(1, 1, 2, 3, 3),
    tstart = c(0, 5, 0, 0, 4),
    tstop = c(5, 9, 6, 4, 10),
    status = c(0, 1, 0, 0, 0)
  )
  surv_subject_truth(
    survival::Surv(long$tstart, long$tstop, long$status),
    subject_id = long$subject_id
  )
}
#> # A tibble: 3 × 3
#>   .subject_id .entry .truth
#>         <dbl>  <dbl> <Surv>
#> 1           1      0     9 
#> 2           2      0     6+
#> 3           3      0    10+
```
