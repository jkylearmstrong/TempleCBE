# Z-Score Standard Normalization

Standardizes numeric features to have mean = 0 and standard deviation
= 1. For a matrix or data frame each (numeric) column is standardized on
its own. A constant column (or a lone observation) has no spread to
divide by and comes back as 0, with `NA`s kept as `NA`. With
`na.rm = FALSE`, a column containing an `NA` has no defined mean and
comes back all `NA`.

## Usage

``` r
z_norm(x, na.rm = TRUE)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame.

- na.rm:

  Logical; whether to ignore NA values (default TRUE).

## Value

Z-score standardized numeric object.

## Examples

``` r
z_norm(c(10, 20, 30, 40, 50))
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
```
