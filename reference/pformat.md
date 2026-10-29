# Format p-values for Biostatistical and Clinical Reporting

Formats numeric p-values into clean, publication-ready strings (e.g.
`"p = 0.024"`, `"p < 0.001"`, or `"0.024"`).

## Usage

``` r
pformat(p, accuracy = 0.001, add_p = TRUE, digits = NULL)

cbe_pformat(p, accuracy = 0.001, add_p = TRUE, digits = NULL)
```

## Arguments

- p:

  Numeric p-value or vector of p-values.

- accuracy:

  Numeric precision threshold passed to
  [`label_pvalue`](https://scales.r-lib.org/reference/label_pvalue.html)
  (default 0.001).

- add_p:

  Logical; if `TRUE` (the default), prepends `"p = "`, `"p < "`, or
  `"p > "`. If `FALSE`, returns only the formatted numeric string.

- digits:

  Optional integer number of decimal places (overrides `accuracy` as
  `10^(-digits)`).

## Value

A character vector of formatted p-values.

## Examples

``` r
pformat(0.0241)
#> [1] "p = 0.024"
pformat(0.0001)
#> [1] "p < 0.001"
pformat(0.852, add_p = FALSE)
#> [1] "0.852"
```
