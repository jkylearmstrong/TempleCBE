# Format Exact 2x2 Odds Ratio and Confidence Interval

Computes the odds ratio and confidence interval from
[`cbe_exact2x2`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_exact2x2.md)
and returns a formatted character string (e.g. `"0.8 (0.3, 2.1)"`).
Useful for inline text reporting or table presentation.

## Usage

``` r
cbe_exact2x2_ci(
  data,
  variable = NULL,
  by = NULL,
  digits = 1,
  conf.level = 0.95,
  midp = NULL,
  ...
)
```

## Arguments

- data:

  A data frame or a 2x2 table/matrix.

- variable:

  Character string column name (if `data` is a data frame).

- by:

  Character string grouping column name (if `data` is a data frame).

- digits:

  Integer number of decimal places for rounding (default 1).

- conf.level:

  Confidence level (default 0.95).

- midp:

  Logical or `NULL` (default). If `NULL`, automatically defaults to
  `TRUE` if any cell count is zero.

- ...:

  Additional arguments passed to
  [`cbe_exact2x2`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_exact2x2.md).

## Value

A character string with the formatted odds ratio and confidence
interval.

## Examples

``` r
tab <- matrix(c(0, 10, 5, 15), nrow = 2)
cbe_exact2x2_ci(tab)
#> [1] "0.0 (0.0, 1.5)"
```
