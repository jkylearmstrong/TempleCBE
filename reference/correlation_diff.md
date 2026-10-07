# Difference in Correlation Matrices Between Two Datasets

Compares the correlation matrix of `comparison_data` against that of
`baseline_data` (e.g. a later timepoint vs. baseline, or a treatment
group vs. a reference group), matching numeric variables by column name.

## Usage

``` r
correlation_diff(
  baseline_data,
  comparison_data,
  cor.use = "everything",
  cor.method = "pearson",
  na_omit = TRUE
)
```

## Arguments

- baseline_data:

  A data frame or tibble treated as the reference.

- comparison_data:

  A data frame or tibble to compare against `baseline_data`.

- cor.use, cor.method, na_omit:

  See
  [`correlation_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md).

## Value

A tibble with one row per shared variable pair, with columns `var1`,
`var2`, and `diff` (comparison correlation minus baseline correlation).

## Details

Unlike PCA loadings (see
[`pca_loading_diff`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff.md)),
correlation coefficients have no sign-ambiguity to correct for – a
correlation matrix is uniquely determined by the data, so the difference
is simply (comparison correlation) minus (baseline correlation), with no
sign-alignment step needed.

Variables are matched by name (numeric column names shared by both
datasets). If the two datasets' numeric columns differ, only the
intersection is used; no error is raised. Because a correlation matrix
is symmetric, only one triangle is returned (no duplicate `var1`/`var2`
vs. `var2`/`var1` rows). The diagonal is dropped: a variable's
correlation with itself is always 1 in both datasets, so its difference
is always 0 and carries no information.

## Examples

``` r
set.seed(1)
baseline <- mtcars
comparison <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
correlation_diff(baseline, comparison)
#> # A tibble: 55 × 3
#>    var1  var2      diff
#>    <chr> <chr>    <dbl>
#>  1 mpg   cyl    0.0566 
#>  2 mpg   disp   0.106  
#>  3 cyl   disp  -0.00686
#>  4 mpg   hp    -0.0282 
#>  5 cyl   hp     0.0144 
#>  6 disp  hp     0.0446 
#>  7 mpg   drat   0.0319 
#>  8 cyl   drat  -0.0417 
#>  9 disp  drat  -0.0603 
#> 10 hp    drat  -0.139  
#> # ℹ 45 more rows
```
