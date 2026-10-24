# Univariate GLM and Mixed-Model Screens, With Coefficient Forest Plots and Tables

\`glm_single()\` and \`lmm_single()\` fit one model per call with a
single predictor, once on the predictor as it is and once rescaled to
the range 0 to 1, so that effects of predictors measured in different
units can be put side by side. \`plot_coef_forest()\` and
\`tbl_coef_forest()\` display the stacked results with significance
stars.

## Usage

``` r
glm_single(
  data,
  outcome,
  feature,
  family = stats::gaussian(),
  normalize = TRUE,
  conf_level = 0.95
)

lmm_single(data, outcome, feature, id_col, normalize = TRUE, conf_level = 0.95)

plot_coef_forest(
  results_df,
  term_col = "var_label",
  estimate_col = "estimate",
  low_col = "conf.low",
  high_col = "conf.high",
  p_col = "p.value",
  p_breaks = c(0.001, 0.01, 0.05, 0.1, 0.2),
  p_labels = c("****", "***", "**", "*", ".", "ns"),
  facet_by = NULL,
  order_by = c("input", "estimate", "p"),
  x_label = NULL
)

tbl_coef_forest(
  results_df,
  term_col = "var_label",
  estimate_col = "estimate",
  low_col = "conf.low",
  high_col = "conf.high",
  p_col = "p.value",
  p_breaks = c(0.001, 0.01, 0.05, 0.1, 0.2),
  p_labels = c("****", "***", "**", "*", ".", "ns"),
  digits = 2
)
```

## Arguments

- data:

  A data frame.

- outcome, feature:

  Names of the outcome and the single predictor.

- family:

  A \[stats::family\] object (default \[stats::gaussian()\]).

- normalize:

  If \`TRUE\` (default) also fit on the 0..1 scaled predictor.

- conf_level:

  Confidence level (default 0.95).

- id_col:

  Name of the grouping (subject) column for the random intercept.

- results_df:

  Results of \[glm_single()\], \[lmm_single()\] (stacked with
  \`rbind()\`/\`dplyr::bind_rows()\`), or any data frame with the
  columns named below.

- term_col, estimate_col, low_col, high_col, p_col:

  Column names (defaults match \`glm_single()\`).

- p_breaks:

  Increasing p-value cut-offs (default \`c(0.001, 0.01, 0.05, 0.1,
  0.2)\`).

- p_labels:

  Labels, one more than \`p_breaks\` (default \`c("\*\*\*\*", "\*\*\*",
  "\*\*", "\*", ".", "ns")\`). A p-value gets the label of the first
  break it is \*\*strictly below\*\*; p at or above the last break gets
  the last label; a missing p-value gets \`""\`.

- facet_by:

  Optional column to facet by (for example \`"scale"\`).

- order_by:

  \`"input"\` (default) keeps the row order, \`"estimate"\` and \`"p"\`
  sort.

- x_label:

  Axis label (default: "Coefficient estimate with confidence interval").

- digits:

  Decimals for the estimate and its interval.

## Value

A tibble of class \`cbe_coef_screen\` with \`feature\`, \`var_label\`,
\`scale\`, \`term\`, \`estimate\`, \`std.error\`, \`statistic\`,
\`p.value\`, \`conf.low\`, \`conf.high\`, \`n\`, \`status\` (\`"ok"\` or
a reason) and \`warning\`. One row per coefficient of the predictor (the
intercept is omitted).

## Scales

\`scale = "raw"\` is the change in the outcome (or in the log odds, log
rate, ...) per \*\*one unit\*\* of the predictor. \`scale =
"normalized"\` is the change across the \*\*whole observed range\*\* of
the predictor (the predictor is min-max scaled to 0..1 with
\[min_max_norm()\]), which makes coefficients of different predictors
comparable in size. A non-numeric predictor has only the raw scale (its
coefficients are the contrasts with the reference level).

## Guards

A predictor with fewer than two distinct non-missing values, or an
outcome with none, returns one row with \`status\` saying so instead of
an error, so a screen over many predictors does not stop. A model that
fails to fit also returns a row with the message; convergence or
singular-fit warnings are kept in \`warning\`.

## Intervals

For \`glm_single()\` with a family whose coefficient table is t-based
(gaussian, quasi), the limits use the t quantile with the residual
degrees of freedom, so the interval excludes 0 exactly when the p-value
is below \`1 - conf_level\`; for z-based families (binomial, poisson)
they use the normal quantile.

## Mixed models

\`lmm_single()\` fits \`outcome ~ feature + (1 \| id_col)\` with
\[lme4::lmer()\] (maximum likelihood is not used for the fixed effects;
REML). Confidence limits and p-values are \*\*Wald\*\* (normal
approximation): \`estimate +/- qnorm(1 - alpha / 2) \* std.error\` and
\`2 \* pnorm(-abs(statistic))\`. They are optimistic for small numbers
of subjects; use \`lmerTest\` or a bootstrap (\[boot_lmer()\]) for small
data. Rows with a missing outcome, predictor or id are dropped.

## See also

\[cox_single()\], \[batch_cox_single()\], \[plot_coef_forest()\]

## Examples

``` r
glm_single(mtcars, "mpg", "wt")
#> # A tibble: 2 × 13
#>   feature var_label scale   term  estimate std.error statistic  p.value conf.low
#>   <chr>   <chr>     <chr>   <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#> 1 wt      wt        raw     wt       -5.34     0.559     -9.56 1.29e-10    -6.49
#> 2 wt      wt        normal… wt      -20.9      2.19      -9.56 1.29e-10   -25.4 
#> # ℹ 4 more variables: conf.high <dbl>, n <int>, status <chr>, warning <chr>
glm_single(mtcars, "am", "wt", family = binomial())
#> # A tibble: 2 × 13
#>   feature var_label scale    term  estimate std.error statistic p.value conf.low
#>   <chr>   <chr>     <chr>    <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>
#> 1 wt      wt        raw      wt       -4.02      1.44     -2.80 0.00509    -6.84
#> 2 wt      wt        normali… wt      -15.7       5.62     -2.80 0.00509   -26.7 
#> # ℹ 4 more variables: conf.high <dbl>, n <int>, status <chr>, warning <chr>
```
