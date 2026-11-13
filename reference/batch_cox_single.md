# Univariate Cox Screening of Many Features

Fits one Cox model per feature (\`Surv(time, status) ~ feature\`, or
\`Surv(start, time, status) ~ feature\` for counting-process data),
optionally in parallel, and returns one consolidated table with hazard
ratios, confidence intervals and a multiple-testing adjusted p-value per
feature. A feature whose model fails is reported with its message
instead of stopping the screen.

## Usage

``` r
batch_cox_single(
  data,
  time = "time",
  status = "status",
  features,
  time_start = NULL,
  adjust_method = "BH",
  exponentiate = TRUE,
  conf_level = 0.95,
  parallel = TRUE
)
```

## Arguments

- data:

  A data frame.

- time, status:

  Names of the follow-up time and event columns (event 1/\`TRUE\`,
  censored 0/\`FALSE\`).

- features:

  Character vector of feature columns.

- time_start:

  Optional name of a start-time column for start-stop (counting process)
  data; \`time\` is then the stop time.

- adjust_method:

  Any method of \[stats::p.adjust()\] (default \`"BH"\`), or \`"none"\`.

- exponentiate:

  If \`TRUE\` (default) report hazard ratios with their limits;
  otherwise raw coefficients (log hazard ratios).

- conf_level:

  Confidence level (default 0.95).

- parallel:

  If \`TRUE\` (default) and the \`furrr\` package is installed, features
  are fitted with \`furrr::future_map()\` under the current
  \[future::plan()\] (sequential unless you set a plan). Without
  \`furrr\` the fits run one after the other.

## Value

A tibble of class \`cbe_batch_cox\` ordered by \`p_adj\`, with
\`feature\`, \`term\`, \`estimate\`, \`conf.high\`, \`std.error\`,
\`statistic\`, \`p.value\`, \`p_overall\`, \`p_adj\`, \`n\`,
\`n_events\`, \`status\` (\`"ok"\`, \`"not estimable"\` for a
coefficient \`coxph\` could not estimate, or the error message) and
\`warning\` (the warnings \`coxph\` raised for the feature, joined with
\`"; "\`, otherwise \`""\`). Look at \`warning\` before trusting a
result: "coefficient may be infinite" means perfect separation, and the
hazard ratio and its p-value are then not meaningful.

## Rows, p-values and adjustment

A numeric or two-level feature gives one row; a factor with \`k\` levels
gives \`k - 1\` rows (one per non-reference level). \`p.value\` is the
Wald p-value of that coefficient. \`p_overall\` is the p-value of the
feature as a whole (the Wald test of all its coefficients,
\`summary(fit)\$waldtest\`) and is the one adjusted: \`p_adj =
p.adjust(p_overall, adjust_method)\` over \*\*features\*\* (each feature
counted once). Features that failed or have no p-value are left out of
the adjustment, so \`p_adj\` is adjusted over the fitted features only.

## See also

\[cox_single()\]

## Examples

``` r
lung <- survival::lung
lung$status <- lung$status - 1
batch_cox_single(lung, "time", "status", c("age", "sex", "ph.ecog", "wt.loss"), parallel = FALSE)
#> # A tibble: 4 × 14
#>   feature term    estimate conf.low conf.high std.error statistic   p.value
#>   <chr>   <chr>      <dbl>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1 ph.ecog ph.ecog    1.61     1.29      2.01    0.113       4.20  0.0000269
#> 2 sex     sex        0.588    0.424     0.816   0.167      -3.18  0.00149  
#> 3 age     age        1.02     1.00      1.04    0.00920     2.03  0.0419   
#> 4 wt.loss wt.loss    1.00     0.989     1.01    0.00608     0.217 0.828    
#> # ℹ 6 more variables: p_overall <dbl>, p_adj <dbl>, n <int>, n_events <dbl>,
#> #   status <chr>, warning <chr>
```
