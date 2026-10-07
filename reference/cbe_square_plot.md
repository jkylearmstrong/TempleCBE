# Square Plot: Standard 4-Quadrant Contingency Report

Dedicated wrapper for
[`cbe_contingency_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_contingency_plot.md)
with `type = "square"`. For 2x2 contingency tables, renders the standard
4-quadrant report:


    q1 | q2
    q3 | q4
    p = pformat

If any cell count is zero, hypothesis testing automatically defaults to
the mid-p version of Central Fisher's exact test via
[`cbe_exact2x2`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_exact2x2.md).

## Usage

``` r
cbe_square_plot(
  data,
  var1 = NULL,
  var2 = NULL,
  label1 = NULL,
  label2 = NULL,
  test = c("auto", "exact", "chisq", "fisher"),
  correct = FALSE,
  engine = c("ggplot", "corrplot"),
  size_range = c(6, 22),
  fill_colors = c("#F4F4F4", "#9D2235"),
  show_counts = TRUE,
  title = NULL,
  caption = NULL,
  p_format = NULL,
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

- test:

  Hypothesis test engine: `"auto"` (default CBE hierarchy), `"exact"`
  (force Central Fisher exact test for 2x2 or simulated Fisher for RxC),
  `"chisq"` (force Pearson's Chi-squared test via
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)), or
  `"fisher"` (force
  [`fisher.test`](https://rdrr.io/r/stats/fisher.test.html)).

- correct:

  Logical; whether to apply continuity correction when `test = "chisq"`
  (default `FALSE` following CBE standard protocol).

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

- caption:

  Optional plot caption. If `NULL`, automatically computed and formatted
  using
  [`pformat`](https://jkylearmstrong.github.io/TempleCBE/reference/pformat.md)
  with test description.

- p_format:

  Optional p-value formatting function (default
  [`pformat`](https://jkylearmstrong.github.io/TempleCBE/reference/pformat.md)).

- ...:

  Additional arguments passed to underlying plotting functions.

## Value

A `ggplot` object or invisibly the contingency table.
