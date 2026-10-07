# Build an igraph From an R Database's Nodes/Edges Tables

The other direction from
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md):
takes a
[`cbe_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
(or plain `list(nodes = ..., edges = ...)`) – such as one produced by
[`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
or the
[`database_setops`](https://jkylearmstrong.github.io/TempleCBE/reference/database_setops.md)
verbs – and builds an `igraph` from it.

## Usage

``` r
database_to_igraph(db, directed = TRUE)
```

## Arguments

- db:

  A `cbe_database` (or plain named list with `nodes` and `edges` data
  frames/tibbles), where `nodes`'s first column is the node identifier
  used by `edges`'s `from`/`to` columns.

- directed:

  Logical, whether the graph is directed (default `TRUE`).

## Value

An `igraph` object.

## Details

Deliberately not a new
[`as_igraph()`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md)
method:
[`as_igraph.default()`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md)
(see
[`as_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md))
already dispatches on plain `list` objects for its own, unrelated
purpose (a list of `FilePath`/ `FileUses`/`FileOutputs` pipeline
objects); adding `as_igraph.list()` here would silently intercept every
one of those calls instead of falling through to `.default`.

## Examples

``` r
db <- as_database(list(
  nodes = data.frame(name = c("a", "b", "c")),
  edges = data.frame(from = c("a", "b"), to = c("b", "c"))
))
database_to_igraph(db)
#> IGRAPH a633f9e DN-- 3 2 -- 
#> + attr: name (v/c)
#> + edges from a633f9e (vertex names):
#> [1] a->b b->c
```
