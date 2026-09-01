# Volcano Plot

Volcano Plot

## Usage

``` r
volcano_plot(
  .data,
  log2_fold_change,
  log_p,
  var,
  alpha = 0.05,
  highlight_significant = TRUE
)
```

## Arguments

- .data:

  A data frame, e.g. the output of
  [`multiple_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/multiple_t_test.md).

- log2_fold_change:

  Unquoted column of log2 fold-change.

- log_p:

  Unquoted column of `-log10(p.value)`.

- var:

  Unquoted column of variable labels.

- alpha:

  Significance threshold for the reference line/shading (default
  `0.05`).

- highlight_significant:

  Logical (default `TRUE`); shade the region above the `p < alpha` line.

## Value

A ggplot object.

## Examples

``` r
mtcars |>
  dplyr::mutate(am = factor(am)) |>
  multiple_t_test(.class = "am") |>
  volcano_plot(log2_fold_change = log2_fold_change, log_p = log_p, var = var)
```
