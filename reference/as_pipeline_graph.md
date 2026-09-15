# Convert pipeline objects or igraph to a tidygraph tbl_graph

Enables native dplyr verbs (activate, filter, mutate, group_by,
summarise) on computational graphs.

## Usage

``` r
as_pipeline_graph(x, ...)
```

## Arguments

- x:

  A list of FilePath/FileUses/FileOutputs objects, or an igraph object.

- ...:

  Additional arguments passed to tidygraph::as_tbl_graph.

## Value

A tbl_graph object.
