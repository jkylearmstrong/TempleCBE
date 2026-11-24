# Screen Many Predictors with Univariable Fine-Gray Models

Runs \[tbl_fine_gray_single()\] for each feature and stacks the results,
with a multiplicity adjusted p-value. A feature that cannot be fitted
(for example zero variance in the rows used) is left out with a warning,
so one bad column does not stop the screen.

## Usage

``` r
tbl_fine_gray_screen(
  data,
  time_col,
  event_col,
  features,
  failcode = 1,
  cencode = 0,
  conf_level = 0.95,
  exponentiate = TRUE,
  p_adjust = "BH",
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

- features:

  Character vector of predictor columns.

- failcode:

  Code of the cause of interest (default 1).

- cencode:

  Code for censoring (default 0).

- conf_level:

  Confidence level.

- exponentiate:

  Report the subdistribution hazard ratio (default); \`FALSE\` gives the
  coefficient on the log scale.

- p_adjust:

  Method for \`stats::p.adjust()\` (default \`"BH"\`), applied across
  all rows.

- ...:

  Passed to \`cmprsk::crr()\` (for example \`maxiter\`).

## Value

The stacked \[tbl_fine_gray_single()\] rows plus \`p.adj\`.

## See also

\[tbl_fine_gray_single()\], \[tbl_fine_gray_multi()\]

## Examples

``` r
if (requireNamespace("cmprsk", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
                  age = rnorm(150, 60, 8), bmi = rnorm(150, 27, 4), arm = rep(c("A", "B"), 75))
  tbl_fine_gray_screen(d, "time", "type", c("age", "bmi", "arm"))
}
#> # A tibble: 3 × 11
#>   feature term  estimate std.error statistic p.value conf.low conf.high     n
#>   <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <int>
#> 1 age     age      1.00     0.0144     0.341   0.733    0.977      1.03   150
#> 2 bmi     bmi      0.973    0.0277    -0.991   0.322    0.921      1.03   150
#> 3 arm     armB     0.827    0.236     -0.805   0.421    0.521      1.31   150
#> # ℹ 2 more variables: n_event <int>, p.adj <dbl>
```
