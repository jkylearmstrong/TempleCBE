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
`p_adjusted`, `conf_low`, `conf_high`. `p_value` and `p_adjusted` are
one-sided (for `importance > 0`); `conf_low` and `conf_high` are a
two-sided interval at level `1 - alpha`. See
[`cbe_loco_mp_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_loco_mp_coxnet.md).
