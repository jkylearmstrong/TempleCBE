# Convert pipeline list to tidygraph

A list of pipeline objects is converted by \[as_pipeline_graph()\]; any
other list is passed on to tidygraph's own list method, which this
method would otherwise replace.

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
