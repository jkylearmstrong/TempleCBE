# Manhattan Plot

Manhattan Plot

## Usage

``` r
manhattan_plot(.data, var, log_p, alpha = 0.05, highlight_significant = TRUE)
```

## Arguments

- .data:

  A data frame, e.g. the output of
  [`multiple_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/multiple_t_test.md).

- var:

  Unquoted column of variable labels.

- log_p:

  Unquoted column of `-log10(p.value)`.

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
  manhattan_plot(var = var, log_p = log_p)
```
