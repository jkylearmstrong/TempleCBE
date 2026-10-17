# Pre-Flight Validation for Counting-Process (Start/Stop) Survival Data

Inspects counting-process survival data before model tuning and reports
subject, interval, event, and censored counts alongside structural
checks for overlapping or gapped intervals, non-positive interval
lengths (\`stop \<= start\`), events occurring before a subject's last
interval, rows appearing after an event, and tied stop/event times.
Emits a prominent warning when no subjects are censored (e.g. 100

## Usage

``` r
validate_start_stop(
  data,
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  warn_no_censoring = TRUE,
  error_on_invalid = FALSE
)
```

## Arguments

- data:

  A data frame containing counting-process survival intervals.

- id, start, stop, status:

  Column names (as strings or bare symbols) for the subject identifier,
  interval start time, interval stop time, and event status indicator
  (\`0\` = censored, \`1\` = event). Defaults to \`"id"\`, \`"tstart"\`,
  \`"tstop"\`, and \`"status"\`.

- warn_no_censoring:

  Logical (default \`TRUE\`). Emit a warning when every subject
  experiences an event (\`n_censored == 0\`).

- error_on_invalid:

  Logical (default \`FALSE\`). When \`TRUE\`, stops with an error if any
  structural issue (missing values, \`stop \<= start\`, overlapping
  intervals, events before the last interval, or rows after an event) is
  detected.

## Value

A 1-row tibble of class \`cbe_start_stop_validation\` with columns:
\`n_subjects\`, \`n_intervals\`, \`n_events\`,
\`n_subjects_with_event\`, \`n_censored\`, \`censoring_rate\`,
\`n_overlapping\`, \`n_gaps\`, \`n_events_before_last\`,
\`n_rows_after_event\`, \`n_nonpositive_length\`, \`n_tied_stop_times\`,
\`n_tied_event_times\`, \`n_missing\`, \`has_censoring\`, and \`valid\`.
Detailed vectors of affected subject IDs are stored in \`attr(x,
"details")\`.

## See also

\[surv_subject_truth()\], \[glmnet_IBS()\]

## Examples

``` r
df <- data.frame(
  id = c(1, 1, 2, 3),
  tstart = c(0, 5, 0, 0),
  tstop = c(5, 10, 8, 12),
  status = c(0, 1, 0, 1)
)
validate_start_stop(df, id = "id", start = "tstart", stop = "tstop", status = "status")
#> TempleCBE Start/Stop Survival Data Check
#> ----------------------------------------
#> Subjects:               3 (2 with event, 1 censored [33.3%])
#> Intervals (rows):       4 (events: 2, missing: 0)
#> Overlapping intervals:  0
#> Gapped intervals:       0
#> Events before last row: 0
#> Rows after an event:    0
#> Non-positive intervals: 0
#> Tied times:             0 stop time(s), 0 event time(s)
#> Status:                 VALID
```
