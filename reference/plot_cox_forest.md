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
  scale = c("hr", "log_hr"),
  color_by = c("none", "significance"),
  order_by = c("none", "magnitude", "pvalue"),
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

  Column name for hazard ratios, always on the HR (not log) scale
  (default: "estimate").

- low_col:

  Column name for lower CI bounds, on the HR scale (default:
  "conf.low").

- high_col:

  Column name for upper CI bounds, on the HR scale (default:
  "conf.high").

- p_col:

  Optional column name for p-values. Required for
  `color_by = "significance"` and `order_by = "pvalue"`.

- color:

  Primary color for points and error bars (default: Temple Cherry
  `"#9D2235"`).

- scale:

  One of `"hr"` (default; plots hazard ratios with a reference line
  at 1) or `"log_hr"` (log-transforms `hr_col`/`low_col`/`high_col` for
  plotting, with a reference line at 0).

- color_by:

  One of `"none"` (default) or `"significance"`, which colors
  points/error bars by whether `p_col` is below 0.05 and adds a legend.

- order_by:

  One of `"none"` (default; original row order), `"magnitude"` (sorts by
  `abs(log(estimate))` descending), or `"pvalue"` (sorts by `p_col`
  ascending).

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

## See also

\[plot_cox_forest_multi()\]
