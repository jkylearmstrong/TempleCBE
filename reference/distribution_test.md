# Check a Vector or Data Frame's Distribution

Runs both
[`is_normal`](https://jkylearmstrong.github.io/TempleCBE/reference/is_normal.md)
and
[`is_poisson`](https://jkylearmstrong.github.io/TempleCBE/reference/is_poisson.md)
against a numeric vector, or against every numeric column of a data
frame.

## Usage

``` r
distribution_test(x)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame/tibble.

## Value

A tibble of test results (with a `feature` column when `x` has multiple
columns).

## Examples

``` r
distribution_test(rpois(n = 1000, lambda = 2))
#> # A tibble: 3 × 8
#>   statistic parameter  p.value method distribution.test p_value_sig distribution
#>       <dbl>     <int>    <dbl> <chr>  <lgl>             <chr>       <chr>       
#> 1     5.08          4 2.79e- 1 Chi-s… TRUE              ""          poisson     
#> 2     0.931        NA 4.29e-21 Shapi… FALSE             "***"       normal      
#> 3     0.179        NA 1.12e-89 Lilli… FALSE             "***"       normal      
#> # ℹ 1 more variable: is_int <lgl>
distribution_test(mtcars)
#> # A tibble: 33 × 9
#>    feature statistic parameter  p.value method     distribution.test p_value_sig
#>    <chr>       <dbl>     <int>    <dbl> <chr>      <lgl>             <chr>      
#>  1 mpg         4.17          4 3.83e- 1 Chi-squar… TRUE              ""         
#>  2 mpg         0.948        NA 1.23e- 1 Shapiro-W… TRUE              ""         
#>  3 mpg         0.126        NA 2.17e- 1 Lilliefor… TRUE              ""         
#>  4 cyl        25.2           4 4.56e- 5 Chi-squar… FALSE             "***"      
#>  5 cyl         0.753        NA 6.06e- 6 Shapiro-W… FALSE             "***"      
#>  6 cyl         0.282        NA 5.80e- 7 Lilliefor… FALSE             "***"      
#>  7 disp       56.5           4 1.57e-11 Chi-squar… FALSE             "***"      
#>  8 disp        0.920        NA 2.08e- 2 Shapiro-W… FALSE             "*"        
#>  9 disp        0.195        NA 3.33e- 3 Lilliefor… FALSE             "**"       
#> 10 hp         51.6           4 1.70e-10 Chi-squar… FALSE             "***"      
#> # ℹ 23 more rows
#> # ℹ 2 more variables: distribution <chr>, is_int <lgl>
```
