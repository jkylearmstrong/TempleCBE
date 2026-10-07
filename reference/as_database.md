# Convert a Graph Object Back Into an R Database

Reverses the `data.base -> graph_object` direction already covered by
[`as_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md)
(and
[`cbe_database_relationships`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_database_relationships.md)):
extracts a graph's node and edge tables into a named list of data frames
(an R database), the same `list(nodes = ..., edges = ...)` shape used
throughout this package's `data.base <-> excel` tooling (see
[`write_workbook`](https://jkylearmstrong.github.io/TempleCBE/reference/write_database_to_excel.md)/[`read_workbook`](https://jkylearmstrong.github.io/TempleCBE/reference/read_workbook.md)).

## Usage

``` r
as_database(x, ...)

# Default S3 method
as_database(x, ...)

# S3 method for class 'igraph'
as_database(x, ...)

# S3 method for class 'visNetwork'
as_database(x, ...)

# S3 method for class 'list'
as_database(x, ...)
```

## Arguments

- x:

  A graph object: an `igraph` (or `tbl_graph`, which extends it and is
  dispatched via the `igraph` method), a visNetwork htmlwidget (e.g.
  from
  [`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html)
  or `funviewR::plot_dependency_graph()`), or a plain
  `list(nodes = ..., edges = ...)` (e.g. read back from
  [`read_workbook`](https://jkylearmstrong.github.io/TempleCBE/reference/read_workbook.md))
  to be validated and stamped as one.

- ...:

  Additional arguments passed to methods.

## Value

A named list `list(nodes = <tibble>, edges = <tibble>)` of class
`cbe_database` – the class
[`database_setops`](https://jkylearmstrong.github.io/TempleCBE/reference/database_setops.md)
and
[`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md)
dispatch on.

## Details

Every node/edge attribute present on `x` is preserved as its own column
– nothing is dropped, renamed, or flattened (e.g. a rich
`FilePath`-derived pipeline graph's `stage`/`mtime`/
`description`/`artifact_role`, see
[`compute_graph`](https://jkylearmstrong.github.io/TempleCBE/reference/compute_graph.md),
all come through intact). There's no merging step here since only one
graph is involved; the "whose attributes win" question only arises once
you combine two databases – see
[`database_setops`](https://jkylearmstrong.github.io/TempleCBE/reference/database_setops.md)
for that.

## Examples

``` r
g <- igraph::make_ring(5)
as_database(g)
#> $nodes
#> # A tibble: 5 × 1
#>   name 
#>   <chr>
#> 1 N_1  
#> 2 N_2  
#> 3 N_3  
#> 4 N_4  
#> 5 N_5  
#> 
#> $edges
#> # A tibble: 5 × 2
#>   from  to   
#>   <chr> <chr>
#> 1 N_1   N_2  
#> 2 N_2   N_3  
#> 3 N_3   N_4  
#> 4 N_4   N_5  
#> 5 N_1   N_5  
#> 
#> attr(,"class")
#> [1] "cbe_database" "list"        
```
