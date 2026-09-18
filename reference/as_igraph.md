# Convert to an enriched igraph graph with rich metadata

Convert to an enriched igraph graph with rich metadata

## Usage

``` r
# S3 method for class 'cbe_database_relationships'
as_igraph(x, ...)

as_igraph(x, ...)

# Default S3 method
as_igraph(x, stage_colors = NULL, ...)
```

## Arguments

- x:

  list of FilePath, FileUses, and FileOutputs objects, or another object
  with an `as_igraph` method (e.g.
  [`cbe_database_relationships`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_database_relationships.md)).

- ...:

  Additional arguments passed to methods.

- stage_colors:

  Optional named list of stage colors. If NULL, uses pipeline_config()
  options.

## Value

an igraph object containing complete node and edge attributes
