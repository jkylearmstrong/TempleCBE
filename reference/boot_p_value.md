# Two-Tailed Bootstrap P-Value

The share of bootstrap estimates on the far side of a null value,
doubled: with \`lower\` the proportion of estimates at or below
\`null_value - epsilon\` and \`upper\` the proportion at or above
\`null_value + epsilon\`, the p-value is \`2 \* min(lower, upper)\`.
Estimates within \`epsilon\` of the null value count on neither side.
Missing estimates are ignored.

## Usage

``` r
boot_p_value(estimates, null_value = 0, epsilon = 1e-05)
```

## Arguments

- estimates:

  Numeric vector of bootstrap estimates.

- null_value:

  The value under the null hypothesis (default 0).

- epsilon:

  Tolerance around the null value (default \`1e-5\`).

## Value

A number between 0 and 1; \`NA\` when there are no estimates. Estimates
that equal the null value exactly count on both sides when \`epsilon\`
is 0, and the result is capped at 1. A p-value of 0 means no estimate
lies on the other side of the null: it says the p-value is below about 2
/ (n + 1), not that it is zero.

## Examples

``` r
boot_p_value(c(0.5, 0.7, 0.9, 1.1, -0.2))
#> [1] 0.4
```
