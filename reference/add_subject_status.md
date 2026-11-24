# Subject-Level Event Status for Start/Stop Data

Adds one event indicator per subject to long (start/stop,
counting-process) data: \`1\` when any of the subject's rows has an
event, \`0\` otherwise. Every row of a subject carries the same value,
so it can stratify grouped folds, e.g. \`rsample::group_vfold_cv(group =
subject_id, strata = subject_status)\`, which keeps the share of
subjects with an event close to equal across folds.

## Usage

``` r
add_subject_status(
  data,
  subject_id,
  status,
  event = 1,
  name = "subject_status"
)
```

## Arguments

- data:

  A data frame with one row per interval.

- subject_id:

  The subject identifier column, as a string.

- status:

  The event column, as a string.

- event:

  The value of \`status\` that marks an event (default \`1\`). A factor
  or character \`status\` is matched on its labels, so a factor with
  levels \`"0"\`/\`"1"\` works as is, and one with levels
  \`c("censored", "event")\` needs \`event = "event"\` (an \`event\`
  that is not a label is an error). A logical \`status\` counts \`TRUE\`
  as the event (\`event\` must be logical, or a value that coerces to
  logical without producing \`NA\`, e.g. \`1\`/\`0\`). A numeric
  \`status\` needs a numeric or logical \`event\`.

- name:

  The name of the new column (default \`"subject_status"\`). It must not
  already exist in \`data\`.

## Value

\`data\` with an integer column \`name\` added.

## See also

\[subject_vfold_cv()\], \[subject_nested_cv()\]

## Examples

``` r
long <- data.frame(id = c(1, 1, 2, 2, 3), status = c(0, 1, 0, 0, 0))
add_subject_status(long, "id", "status")
#>   id status subject_status
#> 1  1      0              1
#> 2  1      1              1
#> 3  2      0              0
#> 4  2      0              0
#> 5  3      0              0
```
