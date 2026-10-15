# Compare Original vs Imputed Empirical Cumulative Distribution Functions

Overlays the empirical cumulative distribution function (ECDF) of
observed versus imputed values for one or more variables.

## Usage

``` r
compare_imputed_ecdf(
  orig,
  imp,
  cols,
  colors = c(Original = "#2B2B2B", Imputed = "#9D2235")
)
```

## Arguments

- orig:

  Data frame containing original (pre-imputation) values.

- imp:

  Data frame containing post-imputation values.

- cols:

  Character vector of variable names to compare.

- colors:

  Named vector of colors for \`c(Original = "...", Imputed = "...")\`.

## Value

A \`ggplot2\` object.

## See also

\[test_imputed_distributions()\], \[plot_imputed_ridgeline()\]

## Examples

``` r
df_orig <- data.frame(x = c(1, 2, NA, 4, 5))
df_imp <- data.frame(x = c(1, 2, 3, 4, 5))
compare_imputed_ecdf(df_orig, df_imp, cols = "x")
```
