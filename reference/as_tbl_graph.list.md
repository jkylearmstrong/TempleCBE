# Convert pipeline list to tidygraph

Convert pipeline list to tidygraph

## Usage

``` r
# S3 method for class 'list'
as_tbl_graph(x, ...)
```

## Arguments

- x:

  A list of FilePath/FileUses/FileOutputs objects.

- ...:

  Additional arguments passed to
  [`as_pipeline_graph`](https://jkylearmstrong.github.io/TempleCBE/reference/as_pipeline_graph.md).

## Value

A tbl_graph object.
