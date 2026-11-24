# Love Plot of Covariate Balance

Plots the absolute standardized mean difference of each covariate, one
dot per sample (unadjusted and adjusted), with dashed lines at the
threshold. Covariates are ordered by their unadjusted imbalance, the
largest at the top.

## Usage

``` r
plot_balance_love(x, threshold = NULL, absolute = TRUE, comparison = NULL, ...)
```

## Arguments

- x:

  A \[balance_smd()\] result, or a data frame; in the second case the
  other arguments of \[balance_smd()\] are taken from \`...\`.

- threshold:

  Where to draw the dashed lines. Default: the threshold stored in \`x\`
  (\`0.1\`).

- absolute:

  Plot \`abs(smd)\` (default \`TRUE\`, the usual Love plot) or the
  signed SMD with lines at plus and minus the threshold.

- comparison:

  For a group with more than two levels, the comparison to show
  (default: all, in facets).

- ...:

  Passed to \[balance_smd()\] when \`x\` is a data frame (\`group\` is
  required).

## Value

A \`ggplot2\` object.

## See also

\[balance_smd()\]

## Examples

``` r
set.seed(1)
d <- data.frame(
  treated = rep(c("no", "yes"), each = 100),
  age = c(rnorm(100, 60, 10), rnorm(100, 66, 10)),
  male = c(rbinom(100, 1, 0.45), rbinom(100, 1, 0.6))
)
plot_balance_love(balance_smd(d, "treated"))
```
