# Is a Vector Composed of Integer-Valued Numbers

Is a Vector Composed of Integer-Valued Numbers

## Usage

``` r
is_integer_valued(col)
```

## Arguments

- col:

  A numeric vector.

## Value

A single logical.

## Examples

``` r
is_integer_valued(sample(-100:100, size = 500, replace = TRUE))
#> [1] TRUE
is_integer_valued(runif(500))
#> [1] FALSE
```
