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

## Details

For a node that is in both graphs, a placeholder counts as missing:
`"Other"` as a `stage`, `"unspecified"` as an `artifact_role`, an empty
string, and `NA`. A graph that merely reads a file (a bare copy of it in
a `dependencies` list) has only placeholders for its stage, role and
description, and must not hide what the graph that produces it says, so
the real value wins whichever graph is given first. When both graphs
hold a real value, the first graph's is kept. The node is `stale` (and
`renders`) if it is in either graph; a node that is stale in only one
graph is drawn as stale, with the stale colour and tooltip. The colour,
shape and tooltip of a node otherwise come from the graph that holds
more real details about it. An edge that both graphs hold (same `from`,
`to` and `type`) is one edge.

Nodes are matched by `name`, as everywhere in the pipeline graph (see
[`as_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md)).
