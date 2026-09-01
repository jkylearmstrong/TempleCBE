# Detect Outliers Across a Data Frame's Numeric Columns

Runs
[`flag_outliers`](https://jkylearmstrong.github.io/TempleCBE/reference/flag_outliers.md)
on every numeric column of `data`.

## Usage

``` r
detect_outliers(data, outliers_only = TRUE)
```

## Arguments

- data:

  A matrix, data frame, or tibble.

- outliers_only:

  Logical (default `TRUE`); if `TRUE`, only rows actually flagged as
  outliers are returned.

## Value

A tibble with a `column` identifying which feature each row came from,
plus `value`, `.outlier`, and `.outlier_type`.

## Examples

``` r
df <- data.frame(a = c(1, 2, 3, 4, 100), b = c(10, 12, 11, 9, 8))
detect_outliers(df)
#> # A tibble: 1 × 4
#>   column value .outlier .outlier_type
#>   <chr>  <dbl> <fct>    <fct>        
#> 1 a        100 TRUE     EXTREME      
detect_outliers(df, outliers_only = FALSE)
#> # A tibble: 10 × 4
#>    column value .outlier .outlier_type
#>    <chr>  <dbl> <fct>    <fct>        
#>  1 a          1 FALSE    NONE         
#>  2 a          2 FALSE    NONE         
#>  3 a          3 FALSE    NONE         
#>  4 a          4 FALSE    NONE         
#>  5 a        100 TRUE     EXTREME      
#>  6 b         10 FALSE    NONE         
#>  7 b         12 FALSE    NONE         
#>  8 b         11 FALSE    NONE         
#>  9 b          9 FALSE    NONE         
#> 10 b          8 FALSE    NONE         
```
