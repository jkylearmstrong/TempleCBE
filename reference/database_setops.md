# Set Operations on R Databases

[`cbe_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
methods for the generic
[`dplyr::intersect()`](https://generics.r-lib.org/reference/setops.html)/
[`dplyr::union()`](https://generics.r-lib.org/reference/setops.html)/[`dplyr::setdiff()`](https://generics.r-lib.org/reference/setops.html)
verbs (the same generics dbplyr/dtplyr extend for their own backends) –
the table-level building blocks behind
[`graph_intersect`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md)/[`graph_subtract`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md)/
[`graph_union`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md).
Given two
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
results (or any plain `list(nodes = ..., edges = ...)` passed through
[`as_database()`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
first), these find their common, exclusive, or combined rows. Operating
at this level (rather than only on graph objects) means they also work
directly on `nodes`/`edges` tables that never came from a graph – e.g.
two tables read back from
[`read_workbook`](https://jkylearmstrong.github.io/TempleCBE/reference/read_workbook.md).

## Arguments

- x, y:

  `cbe_database` objects (see
  [`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)):
  named lists with `nodes` and `edges` data frames/tibbles.

- node_by:

  Column(s) to match nodes on. `NULL` (default) uses the first column of
  each `nodes` table. A single string uses that column name on both
  sides. A named vector/string `c(x_col = y_col)` matches `x`'s `x_col`
  to `y`'s `y_col` (as in `dplyr::*_join()`).

- edge_by:

  Column(s) to match edges on (default `c("from", "to")`, which every
  [`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
  method produces).

- ...:

  Passed on; unused.

## Value

A `cbe_database` `list(nodes = <tibble>, edges = <tibble>)`. For
[`intersect()`](https://generics.r-lib.org/reference/setops.html)/[`setdiff()`](https://generics.r-lib.org/reference/setops.html),
edges are always constrained to reference only the returned nodes, so
the result is a well-formed graph database even if a matched/unmatched
edge's endpoint didn't independently match on the node side.

One consequence of that well-formedness constraint: an edge can vanish
from *both*
[`intersect()`](https://generics.r-lib.org/reference/setops.html) and
[`setdiff()`](https://generics.r-lib.org/reference/setops.html) at once.
If edge `b->c` doesn't literally match one of `y`'s edges, it's excluded
from the intersection – but if node `b` is itself one of `y`'s nodes,
`b` (and anything touching it) also drops out of the setdiff result too,
taking `b->c` with it. The edge belongs to neither side; it isn't
double-dropped or double-counted, just genuinely unrepresentable in a
well-formed result for either operation.

## Details

Dispatch is on the `cbe_database` class (stamped by
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)),
not on `igraph` – igraph already registers its own `union.igraph()` on
this same generic, and S3 method tables are global, so a
`union.igraph()` defined here would silently collide with it.
[`graph_intersect`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md)/[`graph_subtract`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md)/[`graph_union`](https://jkylearmstrong.github.io/TempleCBE/reference/graph_setops.md)
stay dedicated functions for exactly this reason, built on top of these
`cbe_database` methods via
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)/[`database_to_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/database_to_igraph.md).

Node matching defaults to the first column of each database's `nodes`
table – the node identifier column, which different sources name
differently (`name` for an `igraph`-derived database, `id` for a
visNetwork/funviewR one). When the two databases use different column
names, pass `node_by` explicitly using the same `c(x_col = y_col)`
convention as `dplyr::*_join(by = ...)`; a plain string means the same
column name on both sides.

**[`intersect()`](https://generics.r-lib.org/reference/setops.html)/[`setdiff()`](https://generics.r-lib.org/reference/setops.html):
attribute values always come from `x`, never `y`, and are never
merged.**
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)/
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
filter `x`'s rows by whether a key matches in `y` – they don't pull any
of `y`'s own columns across. This matters most for graphs whose nodes
carry rich, node-specific metadata – e.g. a pipeline graph built from
`FilePath`/`FileUses`/ `FileOutputs` objects (see
[`compute_graph`](https://jkylearmstrong.github.io/TempleCBE/reference/compute_graph.md)),
where `stage`, `mtime`, and `description` differ node by node.
Intersecting two snapshots of the same pipeline graph (say, today's vs.
yesterday's) for a node present in both keeps *today's* `mtime`/`stage`,
silently discarding yesterday's – there is no reconciliation between the
two. To compare attribute values themselves (not just which nodes
exist), run
[`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md)
on the two
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
results directly instead.

**[`union()`](https://generics.r-lib.org/reference/setops.html):
attribute values are coalesced, not taken from one side only.** For a
node/edge present in both `x` and `y`, shared columns keep `x`'s value
where it's non-`NA` and fall back to `y`'s otherwise – so combining two
databases never silently drops data the way
[`intersect()`](https://generics.r-lib.org/reference/setops.html)/[`setdiff()`](https://generics.r-lib.org/reference/setops.html)
can. This mirrors
[`join_pipelines`](https://jkylearmstrong.github.io/TempleCBE/reference/join_pipelines.md)'s
existing union-and-coalesce behavior for `tbl_graph` objects,
generalized to any
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
source.

## Examples

``` r
db_x <- as_database(list(
  nodes = data.frame(name = c("a", "b", "c")),
  edges = data.frame(from = c("a", "b"), to = c("b", "c"))
))
db_y <- as_database(list(
  nodes = data.frame(name = c("a", "b")),
  edges = data.frame(from = "a", to = "b")
))
dplyr::intersect(db_x, db_y)
#> $nodes
#> # A tibble: 2 × 1
#>   name 
#>   <chr>
#> 1 a    
#> 2 b    
#> 
#> $edges
#> # A tibble: 1 × 2
#>   from  to   
#>   <chr> <chr>
#> 1 a     b    
#> 
#> attr(,"class")
#> [1] "cbe_database" "list"        
dplyr::setdiff(db_x, db_y)
#> $nodes
#> # A tibble: 1 × 1
#>   name 
#>   <chr>
#> 1 c    
#> 
#> $edges
#> # A tibble: 0 × 2
#> # ℹ 2 variables: from <chr>, to <chr>
#> 
#> attr(,"class")
#> [1] "cbe_database" "list"        
dplyr::union(db_x, db_y)
#> $nodes
#> # A tibble: 3 × 1
#>   name 
#>   <chr>
#> 1 a    
#> 2 b    
#> 3 c    
#> 
#> $edges
#> # A tibble: 2 × 2
#>   from  to   
#>   <chr> <chr>
#> 1 a     b    
#> 2 b     c    
#> 
#> attr(,"class")
#> [1] "cbe_database" "list"        

# intersect()/setdiff(): attribute values come from x only, never merged
# with y's. Node "a" is present in both snapshots below with different
# `stage`/`mtime` (mirroring FilePath's own slots, see compute_graph()) --
# the intersection keeps today's values, not yesterday's.
snapshot_today <- as_database(list(
  nodes = data.frame(
    name = "a", stage = "02_analysis",
    mtime = as.POSIXct("2026-09-18")
  ),
  edges = data.frame(from = character(0), to = character(0))
))
snapshot_yesterday <- as_database(list(
  nodes = data.frame(
    name = "a", stage = "01_eda",
    mtime = as.POSIXct("2026-09-17")
  ),
  edges = data.frame(from = character(0), to = character(0))
))
dplyr::intersect(snapshot_today, snapshot_yesterday)$nodes$stage # "02_analysis"
#> [1] "02_analysis"

# union(): coalesces instead -- yesterday's mtime fills in where a node
# only exists in one snapshot, and today's non-NA values win on overlap.
dplyr::union(snapshot_today, snapshot_yesterday)$nodes
#> # A tibble: 1 × 3
#>   name  stage       mtime              
#>   <chr> <chr>       <dttm>             
#> 1 a     02_analysis 2026-09-18 00:00:00
```
