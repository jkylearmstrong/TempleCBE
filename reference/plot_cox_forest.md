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
  color = "#A41E35",
  scale = c("hr", "log_hr"),
  color_by = c("none", "significance", "p_tier"),
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
  sig_level = 0.05,
  p_breaks = c(0.001, 0.01, 0.05),
  color_by_sig = NULL,
  order_by_magnitude = NULL
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
  `"#A41E35"`).

- scale:

  One of `"hr"` (default; plots hazard ratios with a reference line
  at 1) or `"log_hr"` (log-transforms `hr_col`/`low_col`/`high_col` for
  plotting, with a reference line at 0). Equivalent to
  `exponentiate = TRUE` and `FALSE`; kept for existing code. Give
  `scale` or `exponentiate`, not both with different meanings.

- color_by:

  One of `"none"` (default), `"significance"`, which colors points/error
  bars by whether `p_col` is below `sig_level` (a term is significant
  when `p < sig_level`; a missing p-value counts as not significant) and
  adds a legend, or `"p_tier"`, which colors and shapes the points by
  p-value tier (see `p_breaks`).

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

- p_breaks:

  Increasing p-value cut-offs for \`color_by = "p_tier"\` (default
  \`c(0.001, 0.01, 0.05)\`, at most 5). A term is in the tier of the
  first break its p-value is strictly below; the last tier (open circle,
  grey) is \`p \>=\` the last break and also holds missing p-values.

- color_by_sig, order_by_magnitude:

  Optional logical shortcuts. \`color_by_sig = TRUE\` is \`color_by =
  "p_tier"\` and \`order_by_magnitude = TRUE\` is \`order_by =
  "magnitude"\`; \`FALSE\` is \`"none"\`. \`NULL\` (default) leaves
  \`color_by\` / \`order_by\` as given.

## Value

A ggplot2 object.

## See also

\[plot_cox_forest_multi()\]
