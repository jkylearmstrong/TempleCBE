# Range Normalization

Like
[`min_max_norm`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md),
but treats every numeric column as coming from a single combined
distribution — one global min/max is used to rescale all of them
together, rather than normalizing each column independently. Unlike
`min_max_norm`, non-numeric columns are kept (only the numeric ones are
transformed) — matching the original implementation this was migrated
from.

## Usage

``` r
range_norm(obj)
```

## Arguments

- obj:

  A numeric vector, matrix, data frame, or tibble.

## Value

A normalized object of the same dimensions and class as `obj`.

## Examples

``` r
range_norm(data.frame(x = c(1, 2, 3), y = c(-10, 0, 10)))
#>      x   y
#> 1 0.55 0.0
#> 2 0.60 0.5
#> 3 0.65 1.0
```
