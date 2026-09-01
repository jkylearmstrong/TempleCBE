# Z-Score Standard Normalization

Standardizes numeric features to have mean = 0 and standard deviation =
1.

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
