# Pool Estimates from Several Imputations with Rubin's Rules

Combines the same model fitted to each of \`m\` imputed data sets into
one estimate per term: the mean of the estimates, the within-imputation
variance, the between-imputation variance and the total variance \\T =
\bar{U} + (1 + 1/m) B\\, with Rubin's degrees of freedom and a t-based
interval and p-value.

## Usage

``` r
pool_imputations(
  x,
  imputation_col = "imputation",
  conf_level = 0.95,
  exponentiate = FALSE,
  df_complete = NULL
)
```

## Arguments

- x:

  A list of data frames, or a stacked data frame.

- imputation_col:

  Name of the imputation column when \`x\` is stacked.

- conf_level:

  Confidence level.

- exponentiate:

  Return \`estimate\`, \`conf.low\` and \`conf.high\` exponentiated.

- df_complete:

  Optional complete-data degrees of freedom (for example \`N - k\`).
  When given, the Barnard-Rubin (1999) small-sample adjustment is used,
  \\\nu\_{adj} = (1/\nu + 1/\nu\_{obs})^{-1}\\ with \\\nu\_{obs} =
  \frac{df+1}{df+3} df (1 - \lambda)\\, instead of the large-sample
  \\\nu\\. Terms are pooled over the union of terms across imputations;
  a term missing in any imputation is an error.

## Value

A tibble with \`term\`, \`m\`, \`estimate\`, \`std.error\` (of the
pooled estimate), \`within_var\`, \`between_var\`, \`total_var\`,
\`df\`, \`statistic\`, \`p.value\`, \`conf.low\`, \`conf.high\` and
\`fmi\` (fraction of missing information).

## Input

Either a list of \`m\` data frames (for example \`lapply(imputed,
function(d) broom::tidy(fit(d)))\`) or one stacked data frame with a
column naming the imputation. Each needs \`term\`, \`estimate\` and
\`std.error\`. Pool on the scale the estimates are approximately normal
(log hazard ratio, not hazard ratio); use \`exponentiate = TRUE\` to
report on the ratio scale.

Rubin's rules assume proper multiple imputation (draws from a posterior
predictive distribution). A single imputation, such as one run of
\[impute_piecewise_lmm()\], gives no between-imputation variance, so
impute several times with different seeds first.

## Examples

``` r
set.seed(1)
fits <- lapply(1:5, function(i) {
  data.frame(term = c("x", "z"), estimate = c(0.5, -0.2) + rnorm(2, sd = 0.05),
             std.error = c(0.1, 0.08))
})
pool_imputations(fits)
#> # A tibble: 2 × 13
#>   term      m estimate std.error within_var between_var total_var    df
#>   <chr> <int>    <dbl>     <dbl>      <dbl>       <dbl>     <dbl> <dbl>
#> 1 x         5    0.499    0.106      0.01       0.00110   0.0113  292. 
#> 2 z         5   -0.186    0.0950     0.0064     0.00219   0.00903  47.2
#> # ℹ 5 more variables: statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>, fmi <dbl>
```
