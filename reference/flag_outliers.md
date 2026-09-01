# Flag and Classify Outliers

Flag and Classify Outliers

## Usage

``` r
flag_outliers(col)
```

## Arguments

- col:

  A numeric vector or column.

## Value

A one-column-input-turned-tibble with `value`, `.outlier` (logical) and
`.outlier_type` (factor: `"NONE"`, `"MILD"`, or `"EXTREME"`).

## Examples

``` r
flag_outliers(c(1, 2, 3, 4, 5, 100))
#> # A tibble: 6 × 3
#>   value .outlier .outlier_type
#>   <dbl> <fct>    <fct>        
#> 1     1 FALSE    NONE         
#> 2     2 FALSE    NONE         
#> 3     3 FALSE    NONE         
#> 4     4 FALSE    NONE         
#> 5     5 FALSE    NONE         
#> 6   100 TRUE     EXTREME      
```
