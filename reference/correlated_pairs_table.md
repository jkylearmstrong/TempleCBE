# Formatted Table of Highly Correlated Variable Pairs

Evaluates pairwise correlation between all numeric columns in \`data\`
and returns a tidy table of variable pairs exceeding a correlation
threshold.

## Usage

``` r
correlated_pairs_table(
  data,
  cutoff = 0.5,
  method = c("pearson", "spearman", "kendall"),
  use = c("pairwise.complete.obs", "complete.obs"),
  sort = c("abs_desc", "desc", "p_value", "none")
)
```

## Arguments

- data:

  A data frame or tibble.

- cutoff:

  Minimum absolute correlation threshold (default 0.50). Set to 0 to
  return all pairs.

- method:

  Correlation method: \`"pearson"\` (default), \`"kendall"\`, or
  \`"spearman"\`.

- use:

  Missing value handling passed to \[stats::cor.test\] (default
  \`"pairwise.complete.obs"\`).

- sort:

  Sorting order: \`"abs_desc"\` (default, descending absolute
  correlation), \`"desc"\` (descending raw correlation), \`"p_value"\`
  (ascending p-value), or \`"none"\`.

## Value

A tibble with columns \`var1\`, \`var2\`, \`r\`, \`abs_r\`, \`p_value\`.

## Details

Unlike \[find_correlation()\], which returns column names or indices to
\*drop\* for multicollinearity, \`correlated_pairs_table()\` preserves
and reports the pairwise correlation metrics, absolute magnitude, and
p-values for all pairs above \`cutoff\`.

## See also

\[find_correlation()\], \[corr_test_all()\], \[plot_corr_scatter()\]

## Examples

``` r
correlated_pairs_table(mtcars, cutoff = 0.7)
#> # A tibble: 19 × 5
#>    var1  var2       r abs_r  p_value
#>    <chr> <chr>  <dbl> <dbl>    <dbl>
#>  1 cyl   disp   0.902 0.902 1.80e-12
#>  2 disp  wt     0.888 0.888 1.22e-11
#>  3 mpg   wt    -0.868 0.868 1.29e-10
#>  4 mpg   cyl   -0.852 0.852 6.11e-10
#>  5 mpg   disp  -0.848 0.848 9.38e-10
#>  6 cyl   hp     0.832 0.832 3.48e- 9
#>  7 cyl   vs    -0.811 0.811 1.84e- 8
#>  8 am    gear   0.794 0.794 5.83e- 8
#>  9 disp  hp     0.791 0.791 7.14e- 8
#> 10 cyl   wt     0.782 0.782 1.22e- 7
#> 11 mpg   hp    -0.776 0.776 1.79e- 7
#> 12 hp    carb   0.750 0.750 7.83e- 7
#> 13 qsec  vs     0.745 0.745 1.03e- 6
#> 14 hp    vs    -0.723 0.723 2.94e- 6
#> 15 drat  am     0.713 0.713 4.73e- 6
#> 16 drat  wt    -0.712 0.712 4.78e- 6
#> 17 disp  vs    -0.710 0.710 5.24e- 6
#> 18 disp  drat  -0.710 0.710 5.28e- 6
#> 19 hp    qsec  -0.708 0.708 5.77e- 6
```
