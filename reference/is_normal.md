# Test Whether a Vector Looks Normally Distributed

Runs two normality tests on the finite values of \`col\`:

## Usage

``` r
is_normal(col, alpha = 0.1)
```

## Arguments

- col:

  A numeric vector. \`NA\`, \`NaN\`, and infinite values are dropped.

- alpha:

  Significance level for \`distribution.test\`.

## Value

A tibble with one row per test run: \`statistic\`, \`p.value\`,
\`method\`, \`distribution.test\` (\`TRUE\` when \`p.value \>= alpha\`,
i.e. normality is not rejected), \`p_value_sig\`, and \`distribution\`.
It has no rows when neither test applies (fewer than 3 values, or all
values equal).

## Details

\* \*\*Shapiro-Wilk\*\* (\[stats::shapiro.test()\]), for 3 to 5000
values. It is left out above 5000 rather than run on a random subsample,
so results never depend on the random number generator. \*
\*\*Lilliefors\*\* (\[nortest::lillie.test()\]), for 5 or more values:
the Kolmogorov-Smirnov test corrected for estimating the mean and
standard deviation from the same data. A plain KS test against
\`pnorm(mean(x), sd(x))\` gives p-values that are far too large.

Both tests are deterministic. In large samples they reject normality for
departures too small to matter, so read them alongside a plot such as
\[distribution_plot()\] or a normal Q-Q plot.

## References

Lilliefors HW (1967). On the Kolmogorov-Smirnov test for normality with
mean and variance unknown. \*Journal of the American Statistical
Association\*, 62(318), 399-402.

## Examples

``` r
set.seed(1)
is_normal(rnorm(1000, mean = 5, sd = 3))
#> # A tibble: 2 × 6
#>   statistic p.value method            distribution.test p_value_sig distribution
#>       <dbl>   <dbl> <chr>             <lgl>             <chr>       <chr>       
#> 1    0.999    0.726 Shapiro-Wilk nor… TRUE              ""          normal      
#> 2    0.0178   0.623 Lilliefors (Kolm… TRUE              ""          normal      
is_normal(runif(1000, min = 2, max = 4))
#> # A tibble: 2 × 6
#>   statistic  p.value method           distribution.test p_value_sig distribution
#>       <dbl>    <dbl> <chr>            <lgl>             <chr>       <chr>       
#> 1    0.951  1.20e-17 Shapiro-Wilk no… FALSE             ***         normal      
#> 2    0.0710 6.37e-13 Lilliefors (Kol… FALSE             ***         normal      
```
