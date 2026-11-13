# Mosaic Plot for 2-Way Contingency Tables

Dedicated wrapper for
[`cbe_contingency_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_contingency_plot.md)
with `type = "mosaic"`.

## Usage

``` r
cbe_mosaic_plot(
  data,
  var1 = NULL,
  var2 = NULL,
  label1 = NULL,
  label2 = NULL,
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

- show_counts:

  Logical (default `TRUE`); whether to print cell counts inside plots.

- title:

  Plot title. If `NULL`, defaults to `"{label1} \u00d7 {label2}"`.

- ...:

  Additional arguments passed to underlying plotting functions.

## Value

A `ggplot` object.
