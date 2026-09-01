# Distribution Plot

Ridgeline density plot of every numeric column in `data`, optionally
normalized/standardized first so columns on different scales stay
comparable.

## Usage

``` r
distribution_plot(data, method = "range")
```

## Arguments

- data:

  A data frame or tibble.

- method:

  Normalization applied before plotting: `"range"` (default, via
  [`range_norm`](https://jkylearmstrong.github.io/TempleCBE/reference/range_norm.md)),
  `"min_max"` (via
  [`min_max_norm`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md)),
  `"z"` (via
  [`z_norm`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md)),
  or `"none"`/`"raw"` to plot the values as-is.

## Value

A ggplot object.

## Examples

``` r
df <- data.frame(normal_dist = rnorm(1000, -10, .5), poisson_dist = rpois(1000, 5))
distribution_plot(df)
#> Picking joint bandwidth of 0.0118

distribution_plot(df, method = "z")
#> Picking joint bandwidth of 0.225

distribution_plot(df, method = "none")
#> Picking joint bandwidth of 0.312
```
