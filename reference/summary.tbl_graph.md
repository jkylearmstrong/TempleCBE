# S3 summary method for tbl_graph objects

S3 summary method for tbl_graph objects

## Usage

``` r
# S3 method for class 'tbl_graph'
summary(object, ...)
```

## Arguments

- object:

  A tbl_graph object.

- ...:

  Additional arguments (unused; for S3 method consistency).

## Value

A tibble of the stage breakdown if \`object\` is a pipeline graph (see
[`pipeline_summary`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_summary.md)),
otherwise the default [`summary()`](https://rdrr.io/r/base/summary.html)
for the next applicable class.
