# Scatter Plot of Two Correlated Variables With Regression Fit and Statistics

Visualizes the relationship between two continuous variables with a
linear regression line, confidence interval band, and subtitle/caption
reporting Pearson or Spearman correlation coefficient (r) and p-value.
Replaces inline copies across reports.

## Usage

``` r
plot_corr_scatter(
  data,
  x,
  y,
  method = c("pearson", "spearman"),
  point_color = "#9D2235",
  line_color = "#2B2B2B",
  se = TRUE,
  title = NULL,
  subtitle = NULL,
  base_size = 12
)
```

## Arguments

- data:

  Data frame containing \`x\` and \`y\` columns.

- x:

  Character string naming the x-axis variable.

- y:

  Character string naming the y-axis variable.

- method:

  Correlation method: \`"pearson"\` (default) or \`"spearman"\`.

- point_color:

  Point fill/outline color (default Temple Cherry \`"#9D2235"\`).

- line_color:

  Regression line color (default charcoal \`"#2B2B2B"\`).

- se:

  Logical. Overlay confidence interval band (default \`TRUE\`).

- title:

  Optional plot title.

- subtitle:

  Optional subtitle. If \`NULL\` (default), dynamically formats
  correlation statistic and p-value.

- base_size:

  Base font size (default 12).

## Value

A \`ggplot2\` object.

## See also

\[correlated_pairs_table()\], \[plot_rank_heatmap()\]
