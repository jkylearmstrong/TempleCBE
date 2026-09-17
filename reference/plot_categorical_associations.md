# Plot Categorical Association Matrix Using corrplot

Computes pairwise associations (Cramer's V or -log10 p-values) across
all categorical/factor columns in a data frame and renders an
association matrix plot via corrplot. This complements
[`correlation_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
for numeric variables.

## Usage

``` r
plot_categorical_associations(
  data,
  cols = NULL,
  method = c("cramer_v", "p_value"),
  title = "Categorical Association Matrix",
  fill_colors = c("#FFFFFF", "#9D2235"),
  tl.cex = 0.8,
  number.cex = 0.75,
  show_coef = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame or tibble.

- cols:

  Optional character vector of column names to include. If `NULL`, all
  factor and character columns are selected.

- method:

  Association metric: `"cramer_v"` (Cramer's V correlation, default) or
  `"p_value"` (\\-\log\_{10}(p)\\ from categorical tests).

- title:

  Plot title (defaults to `"Categorical Association Matrix"`).

- fill_colors:

  Character vector for the color gradient (defaults to Temple palette).

- tl.cex:

  Label character expansion (default 0.8).

- number.cex:

  Numeric value character expansion inside circles (default 0.75).

- show_coef:

  Logical (default `TRUE`); whether to print coefficients.

- ...:

  Additional arguments passed to
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html).

## Value

Invisibly, the association matrix.

## Examples

``` r
df <- data.frame(
  A = factor(sample(c("Yes", "No"), 50, replace = TRUE)),
  B = factor(sample(c("High", "Low"), 50, replace = TRUE)),
  C = factor(sample(c("Group1", "Group2", "Group3"), 50, replace = TRUE))
)
plot_categorical_associations(df)
```
