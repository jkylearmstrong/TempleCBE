# Forest Plot of Hazard Ratios with Confidence Intervals

Generates a publication/deck-ready horizontal forest plot of hazard
ratios.

## Usage

``` r
plot_cox_forest(
  data,
  label_col = "index_label",
  hr_col = "estimate",
  low_col = "conf.low",
  high_col = "conf.high",
  p_col = NULL,
  color = "#9D2235",
  x_limits = NULL,
  x_breaks = NULL,
  title = NULL,
  caption = NULL,
  base_size = 13
)
```

## Arguments

- data:

  Data frame containing tidy Cox regression results (e.g. from
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) or
  multi-model runs).

- label_col:

  Column name for labels / terms (character string or unquoted).

- hr_col:

  Column name for hazard ratios (default: "estimate").

- low_col:

  Column name for lower CI bounds (default: "conf.low").

- high_col:

  Column name for upper CI bounds (default: "conf.high").

- p_col:

  Optional column name for p-values to display significance shapes.

- color:

  Primary color for points and error bars (default: Temple Cherry
  `"#9D2235"`).

- x_limits:

  Optional 2-element numeric vector for x-axis limits.

- x_breaks:

  Optional numeric vector of axis break points.

- title:

  Optional plot title.

- caption:

  Optional plot caption.

- base_size:

  Base font size (default: 13).

## Value

A ggplot2 object.
