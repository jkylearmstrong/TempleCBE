# Is a Vector Composed of Integer-Valued Numbers

Is a Vector Composed of Integer-Valued Numbers

## Usage

``` r
is.int(col)
```

## Arguments

- col:

  A numeric vector.

## Value

A single logical.

## Examples

``` r
is.int(sample(-100:100, size = 500, replace = TRUE))
#> [1] TRUE
is.int(runif(500))
#> [1] FALSE
```
