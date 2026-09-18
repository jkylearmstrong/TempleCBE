# Multi-Dataset Key Overlap Venn Diagram

Visualizes shared subject identifiers or key overlap across all tables
in an R database list using ggVennDiagram (or native ggplot2 fallback).

## Usage

``` r
cbe_database_venn(db, id_col = "id", title = NULL, ...)
```

## Arguments

- db:

  A named list of data frames (an R database).

- id_col:

  Character string specifying the identifier column to intersect across
  datasets.

- title:

  Optional plot title.

- ...:

  Additional arguments passed to
  [`ggVennDiagram`](https://gaospecial.github.io/ggVennDiagram/reference/ggVennDiagram.html).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- list(
  inputs = data.frame(id = 1:5),
  abg = data.frame(id = 1:3),
  survival = data.frame(id = 2:6)
)
cbe_database_venn(db, id_col = "id")
} # }
```
