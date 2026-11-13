# Multivariable Fine-Gray Subdistribution Hazard Regression

Fits one Fine-Gray (1999) proportional subdistribution hazards model of
a cause with several predictors at once, using \`cmprsk::crr()\`. It is
the adjusted counterpart of \[tbl_fine_gray_single()\], as
\[cox_multi()\] is for \[cox_single()\]. The estimates are
subdistribution hazard ratios (see \[tbl_fine_gray_single()\] for how to
read them).

## Usage

``` r
tbl_fine_gray_multi(
  data,
  time_col,
  event_col,
  formula,
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

- formula:

  One-sided formula of the predictors, for example \`~ age + arm +
  sex\`.

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

A tibble with \`term\`, \`estimate\`, \`std.error\` (of the log
coefficient), \`statistic\`, \`p.value\`, \`conf.low\`, \`conf.high\`,
\`n\` and \`n_event\`.

## Rows used

Rows with a missing time, event code or any variable in \`formula\` are
dropped (complete cases). The design matrix comes from
\`stats::model.matrix()\`, so factors, logicals, interactions and
transforms such as \`log(x)\` or \`I(x^2)\` work, and terms are named as
\`broom::tidy()\` names them.

## See also

\[tbl_fine_gray_single()\], \[tbl_fine_gray_screen()\],
\[tbl_cumulative_incidence()\]

## Examples

``` r
if (requireNamespace("cmprsk", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
                  age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
  tbl_fine_gray_multi(d, "time", "type", ~ age + arm)
}
#> # A tibble: 2 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high     n n_event
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <int>   <int>
#> 1 age      1.00     0.0141     0.321   0.748    0.977      1.03   150      69
#> 2 armB     0.829    0.236     -0.796   0.426    0.522      1.32   150      69
```
