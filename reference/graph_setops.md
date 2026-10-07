# Intersect, Subtract, or Union Two Graph Objects

Set operations across two graph objects, regardless of their concrete
representation (`igraph`/`tbl_graph`, or a visNetwork htmlwidget such as
`funviewR::plot_dependency_graph()` returns). Both graphs are normalized
via
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md),
combined with the
[`database_setops`](https://jkylearmstrong.github.io/TempleCBE/reference/database_setops.md)
verbs
([`dplyr::intersect()`](https://generics.r-lib.org/reference/setops.html)/
[`dplyr::setdiff()`](https://generics.r-lib.org/reference/setops.html)/[`dplyr::union()`](https://generics.r-lib.org/reference/setops.html)
on the `cbe_database` class), and rebuilt into an `igraph` with
[`database_to_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/database_to_igraph.md).

## Usage

``` r
graph_intersect(x, y, directed = TRUE)

graph_subtract(x, y, directed = TRUE)

graph_union(x, y, directed = TRUE)
```

## Arguments

- x, y:

  Graph objects: `igraph`/`tbl_graph` objects, or visNetwork
  htmlwidgets.

- directed:

  Logical, whether the rebuilt graph is directed (default `TRUE`).

## Value

An `igraph` object. Call
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
on the result to get the underlying node/edge tibbles.

## Details

These stay dedicated functions rather than `intersect.igraph()`/
`union.igraph()`/`setdiff.igraph()` methods because igraph already
registers its own `union.igraph()` on the same generic – S3 method
tables are global, so defining one here would silently collide with
igraph's. Going through
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
first sidesteps that entirely: dispatch happens on the `cbe_database`
class this package owns, not on `igraph`.

`graph_intersect()` keeps nodes/edges present in *both* `x` and `y`,
taking attribute values from `x` only. `graph_subtract()` keeps
nodes/edges present in `x` but *not* in `y` (edges are additionally
constrained to only reference surviving nodes, so the result is always
well-formed). `graph_union()` combines both graphs' nodes and edges,
coalescing attribute values so neither side's data is lost on overlap –
the same union-and-coalesce behavior as
[`join_pipelines`](https://jkylearmstrong.github.io/TempleCBE/reference/join_pipelines.md),
generalized to any graph type via
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
rather than `tbl_graph` specifically.

All three are set operations – which nodes/edges exist where – not a
value-level comparison of attributes on nodes/edges that happen to
match. For that, run
[`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md)
on the two
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
results directly (or on the two graphs themselves).

## Examples

``` r
g1 <- igraph::graph_from_data_frame(
  data.frame(from = c("a", "b"), to = c("b", "c")),
  vertices = data.frame(name = c("a", "b", "c"))
)
g2 <- igraph::graph_from_data_frame(
  data.frame(from = "a", to = "b"),
  vertices = data.frame(name = c("a", "b"))
)
graph_intersect(g1, g2)
#> IGRAPH cbf8df3 DN-- 2 1 -- 
#> + attr: name (v/c)
#> + edge from cbf8df3 (vertex names):
#> [1] a->b
graph_subtract(g1, g2)
#> IGRAPH 9f052f9 DN-- 1 0 -- 
#> + attr: name (v/c)
#> + edges from 9f052f9 (vertex names):
graph_union(g1, g2)
#> IGRAPH dc30de9 DN-- 3 2 -- 
#> + attr: name (v/c)
#> + edges from dc30de9 (vertex names):
#> [1] a->b b->c
```
