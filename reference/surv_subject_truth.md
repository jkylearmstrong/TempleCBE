# Collapse Survival Outcomes to One Row per Subject

Turns right-censored or counting-process (start/stop) survival outcomes
into the one-row-per-subject, right-censored truth that yardstick's
survival metrics require.

## Usage

``` r
surv_subject_truth(truth, subject_id = NULL)
```

## Arguments

- truth:

  A \[survival::Surv()\] object: \`Surv(time, event)\` or \`Surv(start,
  stop, event)\`.

- subject_id:

  Subject identifiers, one per element of \`truth\`. Required for
  counting-process data; for right-censored data each row is its own
  subject when \`subject_id\` is \`NULL\`.

## Value

A tibble with one row per subject, sorted by \`subject_id\`:
\`.subject_id\`, \`.entry\` (start of the first interval; 0 for
right-censored data), and \`.truth\`, a right-censored \`Surv\` object.

## Details

For counting-process data, a subject's time is the stop time of their
last interval and their event status is the status of that interval.
Rows are checked first: within a subject, intervals must not overlap,
and an event may only occur in the last interval (recurrent events are
not supported).

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
