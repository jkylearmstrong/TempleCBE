# Pairwise Correlation Matrix and Significance Testing

Computes pairwise correlations across all numeric features in a data
frame, returning correlation coefficients, p-values, and sample sizes.

## Usage

``` r
corr_test_all(data, method = "pearson", use = "pairwise.complete.obs")
```

## Arguments

- data:

  A data frame or tibble containing numeric variables.

- method:

  Correlation method ("pearson", "kendall", "spearman").

- use:

  Strategy for handling missing values (default
  "pairwise.complete.obs").

## Value

A long tibble with pairs of variables, correlation coefficients (\`r\`),
and p-values (\`p_value\`).

## Examples

``` r
corr_test_all(iris[, 1:4])
#> # A tibble: 6 × 4
#>   var1         var2              r  p_value
#>   <chr>        <chr>         <dbl>    <dbl>
#> 1 Petal.Length Petal.Width   0.963 4.68e-86
#> 2 Petal.Length Sepal.Length  0.872 1.04e-47
#> 3 Petal.Width  Sepal.Length  0.818 2.33e-37
#> 4 Petal.Length Sepal.Width  -0.428 4.51e- 8
#> 5 Petal.Width  Sepal.Width  -0.366 4.07e- 6
#> 6 Sepal.Length Sepal.Width  -0.118 1.52e- 1
```
