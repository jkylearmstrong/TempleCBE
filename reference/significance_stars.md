# P-value Significance Stars

P-value Significance Stars

## Usage

``` r
significance_stars(p_value)
```

## Arguments

- p_value:

  A numeric vector of p-values.

## Value

A character vector of significance stars.

## Examples

``` r
significance_stars(c(0.0001, 0.02, 0.2, 0.8))
#> [1] "***" "*"   ""    ""   
```
