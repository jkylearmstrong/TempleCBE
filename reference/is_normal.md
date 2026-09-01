# Test Whether a Vector Looks Normally Distributed

Runs a Shapiro-Wilk test (for n \<= 5000, on a random subsample above
that) and a one-sample Kolmogorov-Smirnov test against the normal
distribution with mean/sd estimated from `col` (deterministic — no
simulated comparison sample is drawn, so results are reproducible
without seeding).

## Usage

``` r
is_normal(col)
```

## Arguments

- col:

  A numeric vector.

## Value

A tibble of test results.

## Examples

``` r
is_normal(rnorm(1000, mean = 5, sd = 3))
#> # A tibble: 2 × 7
#>   statistic p.value method             alternative distribution.test p_value_sig
#>       <dbl>   <dbl> <chr>              <chr>       <lgl>             <chr>      
#> 1    0.0198   0.826 Asymptotic one-sa… two-sided   TRUE              ""         
#> 2    0.997    0.101 Shapiro-Wilk norm… NA          TRUE              ""         
#> # ℹ 1 more variable: distribution <chr>
is_normal(runif(1000, min = 2, max = 4))
#> # A tibble: 2 × 7
#>   statistic  p.value method            alternative distribution.test p_value_sig
#>       <dbl>    <dbl> <chr>             <chr>       <lgl>             <chr>      
#> 1    0.0636 6.07e- 4 Asymptotic one-s… two-sided   FALSE             ***        
#> 2    0.956  7.91e-17 Shapiro-Wilk nor… NA          FALSE             ***        
#> # ℹ 1 more variable: distribution <chr>
```
