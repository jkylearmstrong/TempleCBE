# Join two computational pipeline graphs

Relational union of two pipeline subgraphs using
tidygraph::graph_join(by = "name"). Nodes with identical names are
coalesced, and their directed dependency and output edges are combined
without duplicating nodes.

## Usage

``` r
join_pipelines(graph_a, graph_b, ...)
```

## Arguments

- graph_a:

  A list of FilePath objects, igraph, or tbl_graph.

- graph_b:

  A list of FilePath objects, igraph, or tbl_graph.

- ...:

  Additional arguments passed to tidygraph::graph_join.

## Value

A merged tbl_graph object.
