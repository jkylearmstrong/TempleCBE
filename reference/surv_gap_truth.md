# Gap-Time Truth for Recurrent-Event Data

Turns counting-process (start/stop) survival outcomes into one row per
\*\*gap\*\* – one row per interval – with the gap time (\`stop -
start\`) and the interval's event status as a right-censored \`Surv\`
outcome. That is the truth that gap-time concordance scores: how well a
model's per-interval risk orders the gaps, from the shortest to the
longest time to the next event.

## Usage

``` r
surv_gap_truth(truth, subject_id = NULL)
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

A tibble with one row per interval, sorted by \`subject_id\` and gap:
\`.subject_id\`, \`.gap\` (the 1-based gap number within the subject),
\`.entry\` (the calendar start of the gap; 0 for right-censored data),
and \`.truth\`, a right-censored \`Surv\` object of gap times.

## Details

Unlike \[surv_subject_truth()\], which collapses each subject to one row
and so can only score the time to first (or last) event, this keeps
every gap. Gaps within a subject are correlated – a subject with a short
first gap tends to have short later gaps – and gap-time concordance
treats them as independent observations, so it describes the ordering of
gaps, not of subjects. There is no recurrent-event Brier score here: a
recurrent IBS (scoring the whole counting process, not one gap at a
time) is follow-up work, not part of this draft.

## See also

\[surv_subject_truth()\], \[cv_coxnet()\], \[glmnet_IBS()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  long <- data.frame(
    subject_id = c(1, 1, 2, 3, 3),
    tstart = c(0, 5, 0, 0, 4),
    tstop = c(5, 9, 6, 4, 10),
    status = c(1, 1, 0, 0, 0)
  )
  surv_gap_truth(
    survival::Surv(long$tstart, long$tstop, long$status),
    subject_id = long$subject_id
  )
}
#> # A tibble: 5 × 4
#>   .subject_id  .gap .entry .truth
#>         <dbl> <int>  <dbl> <Surv>
#> 1           1     1      0     5 
#> 2           1     2      5     4 
#> 3           2     1      0     6+
#> 4           3     1      0     4+
#> 5           3     2      4     6+
```
