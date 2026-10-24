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
  n_cuts = 50
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

## See also

\[plot_km_cutpoints()\], \[km_cutpoint_time_varying()\]

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
}
#> [1] 42.50657
```
