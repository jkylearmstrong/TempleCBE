# Autoplot Method for CBE Data Frame Comparison

Generates ggplot2 visualizations of data frame comparisons: 2-set Venn
diagrams of observation/key overlap or variable concordance (leveraging
ggVennDiagram when installed), or discrepancy bar charts across
variables.

## Usage

``` r
# S3 method for class 'cbe_compare_df'
autoplot(object, type = c("observations", "variables", "discrepancies"), ...)
```

## Arguments

- object:

  A
  [`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md)
  object.

- type:

  Character string specifying the plot type: `"observations"` (default;
  Venn of rows/keys), `"variables"` (Venn of common/unique columns), or
  `"discrepancies"` (bar chart of value differences).

- ...:

  Additional arguments passed to methods or ggVennDiagram.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
