# Forest Plot of Hazard Ratios for a Multivariable Cox Model

Generates a forest plot from a \[cbe_cox_multi()\] result, grouping
terms into per-variable blocks (facets) so factor levels stay visually
attached to their parent variable.

## Usage

``` r
plot_cox_forest_multi(
  x,
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

- x:

  A `cbe_cox_multi` object from \[cbe_cox_multi()\].

- color:

  Primary color for points and error bars (default: Temple Cherry
  `"#9D2235"`).

- scale:

  One of `"hr"` (default) or `"log_hr"`. See \[plot_cox_forest()\].

- color_by:

  One of `"none"` (default) or `"significance"`. See
  \[plot_cox_forest()\].

- order_by:

  One of `"none"` (default), `"magnitude"`, or `"pvalue"`, applied
  within each variable's block. See \[plot_cox_forest()\].

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

\[cbe_cox_multi()\], \[plot_cox_forest()\]
