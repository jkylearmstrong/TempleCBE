# Center a Numeric Vector

Subtracts the mean from a numeric vector, returning a vector (not a 1D
matrix).

## Usage

``` r
center(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector.

- na.rm:

  Logical. Remove missing values when computing the mean (default
  \`TRUE\`).

## Value

Numeric vector centered at zero.

## Examples

``` r
center(c(10, 20, 30))
#> [1] -10   0  10
```
