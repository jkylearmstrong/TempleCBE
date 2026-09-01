# Test Whether a Vector Looks Poisson-Distributed

Runs a chi-squared goodness-of-fit test: observed counts (binned at
roughly equal-probability quantiles of the fitted Poisson distribution,
so expected counts stay adequate regardless of where the data sits) vs.
Poisson-expected counts, with one degree of freedom subtracted for the
estimated rate. The test is deterministic — no simulated comparison
sample is drawn.

## Usage

``` r
is_poisson(col)
```

## Arguments

- col:

  A numeric vector.

## Value

A one-row tibble of test results (empty if `col` isn't valid count data,
e.g. it has negative values).

## Details

A Kolmogorov-Smirnov test is deliberately \*not\* used here: the KS
statistic's null distribution assumes a continuous CDF, and the Poisson
distribution is discrete with real point masses, which inflates the KS
statistic (and deflates its p-value) regardless of true fit — the
chi-squared test is the standard, correctly-calibrated tool for
discrete/count goodness-of-fit.
[`is_normal`](https://jkylearmstrong.github.io/TempleCBE/reference/is_normal.md)
uses a Kolmogorov-Smirnov test because the normal distribution is
continuous.

Since the Poisson distribution's support is the non-negative integers,
this returns an empty tibble for vectors containing negative values or
fewer than 2 observations.

## Examples

``` r
is_poisson(rpois(n = 1000, lambda = 2))
#> # A tibble: 1 × 8
#>   statistic parameter p.value method  distribution.test p_value_sig distribution
#>       <dbl>     <int>   <dbl> <chr>   <lgl>             <chr>       <chr>       
#> 1      2.39         4   0.665 Chi-sq… TRUE              ""          poisson     
#> # ℹ 1 more variable: is_int <lgl>
is_poisson(runif(1000, min = 2, max = 4))
#> # A tibble: 1 × 8
#>   statistic parameter   p.value method             distribution.test p_value_sig
#>       <dbl>     <int>     <dbl> <chr>              <lgl>             <chr>      
#> 1      740.         4 6.35e-159 Chi-squared test … FALSE             ***        
#> # ℹ 2 more variables: distribution <chr>, is_int <lgl>
```
