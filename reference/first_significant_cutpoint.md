# First Significant Cutpoint per Feature

From the result of \[batch_cutpoint_survdiff()\], picks for each feature
and each \`alpha\` the first cutpoint with \`p \< alpha\`, scanning from
the lowest cutpoint when the univariate Cox coefficient is positive and
from the highest otherwise (the most inclusive split on the risk side).

## Usage

``` r
first_significant_cutpoint(x, alpha = c(0.1, 0.05, 0.001))
```

## Arguments

- x:

  Result of \[batch_cutpoint_survdiff()\].

- alpha:

  Significance levels (default \`c(0.1, 0.05, 0.001)\`).

## Value

A tibble with one row per feature and \`alpha\` that has a significant
cutpoint (the columns of \`x\` plus \`alpha\`).

## See also

\[batch_cutpoint_survdiff()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(80), status = rbinom(80, 1, 0.7), x = rnorm(80))
first_significant_cutpoint(batch_cutpoint_survdiff(d, "time", "status", "x"), alpha = 0.5)
#> # A tibble: 1 × 10
#>   feature cutpoint n_low n_high chisq     p p_type   cox_coef cox_p alpha
#>   <chr>      <dbl> <int>  <int> <dbl> <dbl> <ord>       <dbl> <dbl> <dbl>
#> 1 x          -1.46     1     79 0.627 0.428 p >= 0.1  0.00479 0.975   0.5
```
