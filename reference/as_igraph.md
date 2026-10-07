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

## Details

A node is identified by its `name`, not by its `path`. Two objects of
the list with the same name are an error
(`"Duplicate pipeline object name(s) detected"`); a name that comes back
only as a copy inside a `dependencies` or `output` list is one node. A
file declared under two names is two nodes, so staleness does not pass
from one to the other, and one name used for two files is one node. Both
are reported with a warning, once per call, and so are two producers
that declare the same output file.
[`get_render_plan`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md),
[`visualize_pipeline`](https://jkylearmstrong.github.io/TempleCBE/reference/visualize_pipeline.md)
and the summaries built on this function make the same checks.
