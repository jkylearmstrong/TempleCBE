# Grouped, Event-Stratified V-Fold Cross-Validation for Start/Stop Data

\`subject_vfold_cv()\` builds \[rsample::group_vfold_cv()\] folds that
keep all of a subject's intervals in one fold and, when \`status\` is
given, stratify by the subject-level event status of
\[add_subject_status()\]. \`subject_nested_cv()\` does the same for both
the outer and inner folds of an \[rsample::nested_cv()\] object. The
folds work with every start/stop function that takes resamples:
\[nested_cv_coxnet()\], \[cv_coxnet()\], \[tune_grid_start_stop()\],
\[tune_race_start_stop()\] and \[fit_resamples_start_stop()\].

## Usage

``` r
subject_vfold_cv(
  data,
  subject_id,
  status = NULL,
  v = 5,
  repeats = 1,
  event = 1,
  pool = 0.1,
  keep_status = FALSE,
  ...
)

subject_nested_cv(
  data,
  subject_id,
  status = NULL,
  outer_v = 5,
  inner_v = 5,
  repeats = 1,
  event = 1,
  pool = 0.1,
  keep_status = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame with one row per interval.

- subject_id:

  The subject identifier column, as a string.

- status:

  The event column, as a string, or \`NULL\` (default) for grouped folds
  without stratification.

- v:

  The number of folds. \`subject_nested_cv()\` takes \`outer_v\` and
  \`inner_v\`.

- repeats:

  The number of repeats of the outer folds.

- event:

  The value of \`status\` that marks an event (default \`1\`). A factor
  or character \`status\` is matched on its labels, so a factor with
  levels \`"0"\`/\`"1"\` works as is, and one with levels
  \`c("censored", "event")\` needs \`event = "event"\` (an \`event\`
  that is not a label is an error). A logical \`status\` counts \`TRUE\`
  as the event (\`event\` must be logical, or a value that coerces to
  logical without producing \`NA\`, e.g. \`1\`/\`0\`). A numeric
  \`status\` needs a numeric or logical \`event\`.

- pool:

  Passed to \[rsample::group_vfold_cv()\]: strata holding fewer than
  this share of the subjects are pooled.

- keep_status:

  Keep the helper column in the splits' data: \`subject_status\`, or
  \`.subject_status\` if a column of that name already exists in
  \`data\` (the existing column is left in place).

- ...:

  Passed to \[rsample::group_vfold_cv()\] (to both the outer and the
  inner folds in \`subject_nested_cv()\`). In practice only \`balance\`
  is forwarded usefully, as \`rsample\` accepts no other extras.
  \`group\` and \`strata\` cannot be passed through \`...\`: they are
  set by these functions (and \`v\` is not a \`subject_nested_cv()\`
  argument either). Pass \`v\`, \`repeats\` and \`pool\` —
  \`outer_v\`/\`inner_v\` for \`subject_nested_cv()\` — as direct
  arguments instead.

- outer_v, inner_v:

  The number of outer and inner folds.

## Value

An \`rset\` (\`subject_vfold_cv()\`) or a \`nested_cv\` object
(\`subject_nested_cv()\`).

## Details

The stratification column only exists while the folds are built: with
\`keep_status = FALSE\` (default) it is removed from the data the splits
hold, so a \`y ~ .\` formula or recipe does not pick it up as a
predictor. If \`data\` already has a \`subject_status\` column, the
helper is named \`.subject_status\` instead and the existing column is
left in place.

## See also

\[add_subject_status()\]

## Examples

``` r
# \donttest{
if (requireNamespace("rsample", quietly = TRUE)) {
  set.seed(1)
  long <- data.frame(
    id = rep(1:40, each = 2), start = rep(c(0, 1), 40), stop = rep(c(1, 2), 40),
    status = rep(c(0, 1), 40) * rbinom(80, 1, 0.5), x = rnorm(80)
  )
  folds <- subject_vfold_cv(long, "id", status = "status", v = 4)
  nested <- subject_nested_cv(long, "id", status = "status", outer_v = 4, inner_v = 3)
}
# }
```
