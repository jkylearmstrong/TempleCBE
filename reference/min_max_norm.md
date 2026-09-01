# Min-Max Data Normalization

Normalizes numerical values into `[0, 1]`. For a data frame, matrix, or
tibble, each numeric column is normalized independently against its own
min/max, and **non-numeric columns are dropped** — matching the original
implementation this was migrated from.

## Usage

``` r
min_max_norm(obj)
```

## Arguments

- obj:

  A numeric vector, matrix, data frame, or tibble.

## Value

A normalized object: a numeric vector stays a vector; a matrix stays a
matrix; a data frame or tibble is reduced to just its (now normalized)
numeric columns.

## Examples

``` r
min_max_norm(c(10, 20, 30, 40, 50))
#> [1] 0.00 0.25 0.50 0.75 1.00
min_max_norm(data.frame(x = c(1, 2, 3), y = c(-10, 0, 10), id = c("a", "b", "c")))
#>     x   y
#> 1 0.0 0.0
#> 2 0.5 0.5
#> 3 1.0 1.0
```
