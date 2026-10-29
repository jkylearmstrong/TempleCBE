# Balloon Plot for 2-Way Contingency Tables

Dedicated wrapper for
[`cbe_contingency_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_contingency_plot.md)
with `type = "balloon"`.

## Usage

``` r
cbe_balloon_plot(
  data,
  var1 = NULL,
  var2 = NULL,
  label1 = NULL,
  label2 = NULL,
  engine = c("ggplot", "corrplot", "ggpubr"),
  size_range = c(6, 22),
  fill_colors = c("#F4F4F4", "#9D2235"),
  show_counts = TRUE,
  title = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the categorical variables, or a 2-way
  table/matrix.

- var1:

  Character string of the row variable column name (ignored if `data` is
  a table).

- var2:

  Character string of the column variable column name (ignored if `data`
  is a table).

- label1:

  Optional display label for `var1` (defaults to `var1`).

- label2:

  Optional display label for `var2` (defaults to `var2`).

- engine:

  Engine for balloon/square rendering: `"ggplot"` (default),
  `"corrplot"`, or `"ggpubr"`.

- size_range:

  Numeric vector of length 2 giving min and max glyph sizes for balloon
  and square plots (default `c(6, 22)`).

- fill_colors:

  Character vector of colors for the palette or gradient. Defaults to
  Temple University brand palette (`c("#F4F4F4", "#9D2235")`).

- show_counts:

  Logical (default `TRUE`); whether to print cell counts inside plots.

- title:

  Plot title. If `NULL`, defaults to `"{label1} \u00d7 {label2}"`.

- ...:

  Additional arguments passed to underlying plotting functions.

## Value

A `ggplot` object or invisibly the contingency table.
