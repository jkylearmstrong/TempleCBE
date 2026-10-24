# Statistical Distribution Comparison of Original vs Imputed Values

Runs Kolmogorov-Smirnov (KS) tests and two-sample comparisons on
observed vs imputed values across specified variables, reporting test
statistics and p-values.

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
tests.

## See also

\[compare_imputed_ecdf()\]
