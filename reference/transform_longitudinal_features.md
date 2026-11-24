# Per-Subject Summaries of Repeated Measurements

Collapses each subject's repeated values of a feature into one number,
using one or more transformations that are often tried as predictors of
an outcome: change from baseline, within-subject variability, or the
largest swing.

## Usage

``` r
transform_longitudinal_features(
  data,
  id_col,
  time_col,
  features,
  methods = c("pct_change_baseline", "variance", "sd", "max_signed_diff"),
  format = c("long", "wide")
)
```

## Arguments

- data:

  A data frame in long format (one row per subject and time).

- id_col, time_col:

  Names of the subject and numeric time columns.

- features:

  Character vector of numeric feature columns.

- methods:

  One or more of \`"pct_change_baseline"\`, \`"variance"\`, \`"sd"\`,
  \`"max_signed_diff"\`.

- format:

  \`"long"\` (default; one row per subject, feature and method) or
  \`"wide"\` (one row per subject, columns named
  \`\<feature\>\_\_\<method\>\`).

## Value

A tibble. Long format: the id column (named as \`id_col\`), \`feature\`,
\`method\`, \`value\`, \`n_obs\`. Wide format: the id column and the
value columns.

## Definitions

Within a subject and feature, missing values are dropped and the rest
are ordered by \`time_col\` (ties in time keep the data order).
\`n_obs\` is the number of values used. \* \`"pct_change_baseline"\`:
\`100 \* (last - first) / abs(first)\`, with \`first\` the value at the
earliest time (the baseline) and \`last\` the value at the latest time;
the absolute value keeps the sign meaning "went up" or "went down" when
the baseline is negative. \`NA\` if the baseline is 0 or there is only
one value. \* \`"variance"\` and \`"sd"\`: the sample variance and
standard deviation (divisor \`n - 1\`) of all values; \`NA\` with fewer
than two values. \* \`"max_signed_diff"\`: \`max - min\` of the values,
\*\*signed by time order\*\*: positive when the (first) maximum comes
after the (first) minimum, negative when it comes before, and 0 when all
values are equal. \`NA\` with fewer than two values.

## See also

\[batch_cox_single()\], \[plot_feature_transform_heatmap()\]

## Examples

``` r
long <- data.frame(id = rep(1:3, each = 3), time = rep(c(0, 5, 15), 3),
                   hr = c(80, 90, 70, 60, 61, 62, 100, 90, 80))
transform_longitudinal_features(long, "id", "time", "hr")
#> # A tibble: 12 × 5
#>       id feature method               value n_obs
#>    <int> <chr>   <chr>                <dbl> <int>
#>  1     1 hr      pct_change_baseline -12.5      3
#>  2     1 hr      variance            100        3
#>  3     1 hr      sd                   10        3
#>  4     1 hr      max_signed_diff     -20        3
#>  5     2 hr      pct_change_baseline   3.33     3
#>  6     2 hr      variance              1        3
#>  7     2 hr      sd                    1        3
#>  8     2 hr      max_signed_diff       2        3
#>  9     3 hr      pct_change_baseline -20        3
#> 10     3 hr      variance            100        3
#> 11     3 hr      sd                   10        3
#> 12     3 hr      max_signed_diff     -20        3
```
