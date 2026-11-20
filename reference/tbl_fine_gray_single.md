# Fine-Gray Subdistribution Hazard Regression for One Predictor

Fits the Fine-Gray (1999) proportional subdistribution hazards model of
one cause with the other causes treated as competing events, for one
predictor at a time. It is the regression counterpart of
\[tbl_cumulative_incidence()\], as \[cox_single()\] is for
\[km_single()\]. Uses \`cmprsk::crr()\`.

## Usage

``` r
tbl_fine_gray_single(
  data,
  time_col,
  event_col,
  feature,
  failcode = 1,
  cencode = 0,
  conf_level = 0.95,
  exponentiate = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame, one row per subject.

- time_col:

  Follow-up time.

- event_col:

  Event type, as in \[tbl_cumulative_incidence()\]: a censoring code,
  then one code per cause.

- feature:

  Name of the predictor column.

- failcode:

  Code of the cause of interest (default 1).

- cencode:

  Code for censoring (default 0).

- conf_level:

  Confidence level.

- exponentiate:

  Report the subdistribution hazard ratio (default); \`FALSE\` gives the
  coefficient on the log scale.

- ...:

  Passed to \`cmprsk::crr()\` (for example \`maxiter\`).

## Value

A tibble with \`feature\`, \`term\`, \`estimate\`, \`std.error\` (of the
log coefficient), \`statistic\`, \`p.value\`, \`conf.low\`,
\`conf.high\`, \`n\` and \`n_event\` (events of the cause of interest
among the rows used).

## Interpretation

The estimate is a subdistribution hazard ratio (sHR). It describes the
effect on the cumulative incidence of the cause of interest, not on the
rate among those still event-free, so it is not comparable to a
cause-specific hazard ratio from \[cox_single()\] with the competing
events censored. Subjects with a competing event stay in the risk set,
which is what \`crr()\` does. Rows with a missing time, event code or
predictor are dropped. A factor or character predictor gets one row per
non-reference level (the first level is the reference). Terms are named
as \`broom::tidy()\` names them: a logical predictor gives
\`\<feature\>TRUE\`, a factor \`\<feature\>\<level\>\`.

## See also

\[tbl_cumulative_incidence()\], \[plot_cumulative_incidence()\],
\[cox_single()\]

## Examples

``` r
if (requireNamespace("cmprsk", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
                  age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
  tbl_fine_gray_single(d, "time", "type", "age")
  tbl_fine_gray_single(d, "time", "type", "arm", failcode = 2)
}
#> # A tibble: 1 × 10
#>   feature term  estimate std.error statistic p.value conf.low conf.high     n
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <int>
#> 1 arm     armB      1.50     0.328      1.24   0.216    0.789      2.86   150
#> # ℹ 1 more variable: n_event <int>
```
