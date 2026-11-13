# Search for Optimal Survival Cutpoint via Log-Rank Separation

Evaluates candidate thresholds of a continuous variable to stratify
patients into low-risk and high-risk groups. Evaluates log-rank test
statistics and univariable Cox hazard ratios across candidate
percentiles to identify the optimal stratification.

## Usage

``` r
km_cutpoint_search(
  data,
  time_col,
  status_col,
  var_col,
  min_prop = 0.1,
  n_cuts = 50,
  cutpoints = c("quantiles", "midpoints"),
  alpha = c(0.1, 0.05, 0.001)
)
```

## Arguments

- data:

  Data frame containing survival follow-up and biomarker columns.

- time_col:

  Character string naming the follow-up time column.

- status_col:

  Character string naming the event status column (1 = event, 0 =
  censored).

- var_col:

  Character string naming the continuous biomarker or index variable.

- min_prop:

  Minimum proportion of subjects required in each stratified subgroup
  (default 0.10).

- n_cuts:

  Number of quantile evaluation points between \`min_prop\` and \`1 -
  min_prop\` (default 50).

- cutpoints:

  Candidate cutpoints: \`"quantiles"\` (default; \`n_cuts\` quantiles
  between \`min_prop\` and \`1 - min_prop\`) or \`"midpoints"\` (every
  midpoint between consecutive distinct values, still subject to
  \`min_prop\`).

- alpha:

  Significance levels for the \`first_p\<alpha\` criteria (default
  \`c(0.1, 0.05, 0.001)\`; \`NULL\` for none).

## Value

A list with class \`"cbe_km_cutpoint"\` containing:

- optimal_cutpoint:

  Numeric value of the optimal cutpoint threshold

- optimal_p:

  P-value at the optimal cutpoint

- optimal_hr:

  Hazard ratio of high vs low group at the optimal cutpoint

- candidates:

  A tibble of evaluated candidate cutpoints and statistics

- criteria:

  A tibble with one row per selection criterion (see Selection
  criteria): \`criterion\`, the columns of \`candidates\`, and
  \`neg_log10_p\`

- cox_coef:

  Coefficient of the continuous variable in a univariable Cox model

- data:

  The input data frame augmented with a `<var_col>_strata` binary factor
  column

- time_col:

  Name of time column

- status_col:

  Name of status column

- var_col:

  Name of biomarker column

## Details

Replaces inline cutpoint selection scripts across clinical survival
reports.

## Selection criteria

\`criteria\` has one row per rule, each a split of the cohort at \`value
\> cutpoint\`: \* \`median\`: the median of the variable (scored even
when it is not a candidate). \* \`maxstat\`: the maximally selected
log-rank statistic of \`maxstat::maxstat.test()\`, which is what
\`survminer::surv_cutpoint()\` reports. Present only when the maxstat
package is installed. \* \`max_logrank\`: the candidate with the largest
log-rank chi-square. The optimum of the object (\`optimal_cutpoint\`) is
this row. \* \`first_p\<alpha\`, one per value of \`alpha\`: the first
candidate with a p-value below \`alpha\`, scanning up from the lowest
candidate when the Cox coefficient of the continuous variable is
positive and down from the highest otherwise (the most inclusive split
on the risk side). Absent when no candidate reaches that level. \*
\`lowest_p\`: the candidate with the smallest p-value (the same split as
\`max_logrank\` for right-censored data, where the p-value is a function
of the chi-square).

All of these except \`median\` choose the cutpoint by looking at the
outcome, so their p-values and hazard ratios are optimistic and are not
valid tests; report them as exploratory or validate the cutpoint in
other data.

## See also

\[plot_km_cutpoints()\], \[plot_km_cutpoint()\],
\[plot_cutpoint_bar()\], \[km_cutpoint_time_varying()\],
\[batch_cutpoint_survdiff()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  set.seed(123)
  df <- data.frame(
    time = stats::rexp(100, 0.1),
    status = stats::rbinom(100, 1, 0.7),
    marker = stats::rnorm(100, 50, 10)
  )
  res <- km_cutpoint_search(df, "time", "status", "marker")
  res$optimal_cutpoint
  res$criteria
}
#> # A tibble: 4 × 10
#>   criterion   cutpoint n_low n_high  chisq p_value    hr conf_low conf_high
#>   <fct>          <dbl> <int>  <int>  <dbl>   <dbl> <dbl>    <dbl>     <dbl>
#> 1 median          47.7    50     50 0.0264   0.871  1.04    0.649      1.67
#> 2 maxstat         37.6    13     87 1.37     0.241  1.56    0.738      3.29
#> 3 max_logrank     42.5    23     77 1.34     0.248  1.40    0.789      2.49
#> 4 lowest_p        42.5    23     77 1.34     0.248  1.40    0.789      2.49
#> # ℹ 1 more variable: neg_log10_p <dbl>
```
