# Pool Model-Level Fit Statistics over Imputations

Companion of \[pool_imputations()\] for the one-row-per-model summaries
(\`broom::glance()\`, the \`glance\` of \[cox_single()\]): concordance,
Brier or integrated Brier score, AIC, degrees of freedom and so on, over
the same model fitted to each of \`m\` imputed data sets.

## Usage

``` r
glance_imputations(
  x,
  imputation_col = "imputation",
  conf_level = 0.95,
  df_complete = NULL
)
```

## Arguments

- x:

  A list of one-row data frames (or tibbles), or a stacked data frame
  with a column naming the imputation.

- imputation_col:

  Name of the imputation column when \`x\` is stacked.

- conf_level:

  Confidence level for statistics that have a standard error.

- df_complete:

  Complete-data degrees of freedom (for example \`N - k\`) for the
  Barnard-Rubin (1999) small-sample adjustment of the interval, passed
  to \[pool_imputations()\]. \`NULL\` (default) uses the large-sample
  Rubin degrees of freedom. Only affects statistics that have a
  \`std.error.\<stat\>\` column.

## Value

A tibble with \`statistic\`, \`m\`, \`estimate\`, \`between_sd\`,
\`min\`, \`max\`, and, where a standard error was available,
\`std.error\`, \`conf.low\`, \`conf.high\` and \`fmi\` (otherwise
\`NA\`).

## Method

Every numeric column is pooled as the mean over the imputations,
reported with the between-imputation standard deviation, minimum and
maximum. This is the usual summary for fit statistics, which have no
Rubin variance of their own. If the column \`std.error.\<stat\>\` exists
(as \`broom::glance()\` gives for \`concordance\`), the statistic is
instead pooled with Rubin's rules through \[pool_imputations()\], giving
a standard error, a t interval and the fraction of missing information.
Pooled concordance is a mean of C-indices, each computed on its own
completed data set; it is not a C-index of a pooled model. Columns named
\`std.error.\*\` are not reported as statistics.

## See also

\[pool_imputations()\]

## Examples

``` r
set.seed(1)
g <- lapply(1:5, function(i) data.frame(concordance = 0.7 + rnorm(1, sd = 0.01),
                                        std.error.concordance = 0.03, AIC = 400 + rnorm(1, sd = 2),
                                        df = 3))
glance_imputations(g)
#> # A tibble: 3 × 10
#>   statistic       m estimate between_sd     min     max std.error conf.low
#>   <chr>       <int>    <dbl>      <dbl>   <dbl>   <dbl>     <dbl>    <dbl>
#> 1 concordance     5    0.700    0.00665   0.692   0.706    0.0309    0.639
#> 2 AIC             5  401.       1.87    398.    403.      NA        NA    
#> 3 df              5    3        0         3       3       NA        NA    
#> # ℹ 2 more variables: conf.high <dbl>, fmi <dbl>
```
