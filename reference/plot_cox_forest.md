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
  base_size = 13,
  label_map = NULL,
  style = NULL,
  exponentiate = NULL,
  log_scale = FALSE,
  sig_level = 0.05
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
  plotting, with a reference line at 0). Equivalent to
  `exponentiate = TRUE` and `FALSE`; kept for existing code. Give
  `scale` or `exponentiate`, not both with different meanings.

- color_by:

  One of `"none"` (default) or `"significance"`, which colors
  points/error bars by whether `p_col` is below `sig_level` (a term is
  significant when `p < sig_level`; a missing p-value counts as not
  significant) and adds a legend.

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

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

- exponentiate:

  \`NULL\` (default: follow \`scale\`), \`TRUE\` to show hazard ratios
  (reference line at 1) or \`FALSE\` to show the raw coefficients,
  \`log(HR)\` (reference line at 0, linear axis). The columns you pass
  are always on the HR scale; this only chooses what is displayed, and
  the confidence limits are transformed with the estimate.

- log_scale:

  Logical. With hazard ratios, \`TRUE\` draws the axis on a log scale
  (reference line still at 1, so equal ratios in either direction are
  the same distance from it). This is a separate choice from
  \`exponentiate\`; \`log_scale = TRUE\` with \`exponentiate = FALSE\`
  is an error, because the coefficients are already logs. All HR and
  confidence limits must be positive.

- sig_level:

  Significance threshold for \`color_by = "significance"\` (default
  0.05).

## Value

A ggplot2 object.

## See also

\[plot_cox_forest_multi()\]
