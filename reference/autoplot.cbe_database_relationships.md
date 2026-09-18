# Autoplot Method for Database Relationships

Autoplot Method for Database Relationships

## Usage

``` r
# S3 method for class 'cbe_database_relationships'
autoplot(object, type = c("graph", "venn"), db = NULL, id_col = NULL, ...)
```

## Arguments

- object:

  A
  [`cbe_database_relationships`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_database_relationships.md)
  object.

- type:

  Plot type: `"graph"` for network ER graph, or `"venn"` for key
  overlap.

- db:

  Optional parent database list required when `type = "venn"`.

- id_col:

  Identifier column for Venn overlap (default: first shared key).

- ...:

  Additional arguments passed to plotting backends.

## Value

A ggplot2 or plot object.
