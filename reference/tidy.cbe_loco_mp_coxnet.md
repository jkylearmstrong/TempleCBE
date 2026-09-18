# Tidy a LOCO-MP Cox Model Object

Extracts feature importance and inference metrics into a clean tibble.

## Usage

``` r
# S3 method for class 'cbe_loco_mp_coxnet'
tidy(x, ...)
```

## Arguments

- x:

  A `cbe_loco_mp_coxnet` object.

- ...:

  Not used.

## Value

A tibble with `term`, `importance`, `std_error`, `statistic`, `p_value`,
`p_adjusted`, `conf_low`, `conf_high`.
