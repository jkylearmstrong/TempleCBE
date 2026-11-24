# Statistical Distribution Comparison of Original vs Imputed Values

Runs Kolmogorov-Smirnov (KS) tests and two-sample comparisons on
observed vs imputed values across specified variables, reporting test
statistics and p-values. The variance of the imputed values over the
variance of the observed ones (\`variance_ratio\`) shows whether the
imputation shrinks the spread: a ratio well below 1 is the usual sign of
an imputation that fills in values near the center.

## Usage

``` r
test_imputed_distributions(orig, imp, cols)
```

## Arguments

- orig:

  Data frame containing original (pre-imputation) values.

- imp:

  Data frame containing post-imputation values.

- cols:

  Character vector of variable names to test.

## Value

A tibble with one row per variable summarizing distribution comparison
tests: \`n_observed\`, \`n_imputed\`, \`mean_observed\`,
\`mean_imputed\`, \`ks_statistic\`, \`ks_p_value\`, \`t_p_value\`,
\`var_observed\`, \`var_imputed\`, \`variance_ratio\` (imputed over
observed) and \`var_p_value\` (F test of equal variances). The variance
columns are \`NA\` for non-numeric variables, for variables without
imputed values and for fewer than two values.

## See also

\[compare_imputed_ecdf()\], \[plot_imputed_ridgeline()\]

## Examples

``` r
set.seed(1)
df_orig <- data.frame(x = c(rnorm(40), rep(NA, 10)))
df_imp <- df_orig
df_imp$x[is.na(df_imp$x)] <- mean(df_orig$x, na.rm = TRUE)
test_imputed_distributions(df_orig, df_imp, "x")  # mean imputation: variance_ratio is 0 (NA test)
#> # A tibble: 1 × 12
#>   variable n_observed n_imputed mean_observed mean_imputed ks_statistic
#>   <chr>         <int>     <int>         <dbl>        <dbl>        <dbl>
#> 1 x                40        10        0.0920       0.0920          0.5
#> # ℹ 6 more variables: ks_p_value <dbl>, t_p_value <dbl>, var_observed <dbl>,
#> #   var_imputed <dbl>, variance_ratio <dbl>, var_p_value <dbl>
```
