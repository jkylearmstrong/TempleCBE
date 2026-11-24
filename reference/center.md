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

## Details

\`center\` is a common name: other attached packages, or an object of
your own called \`center\`, can mask it (and
\[compute_composite_index()\] has an argument of that name). In scripts,
write \`TempleCBE::center()\` to be sure which one is called.

## Examples

``` r
center(c(10, 20, 30))
#> [1] -10   0  10
```
