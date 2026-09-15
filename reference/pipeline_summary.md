# Produce an executive summary of the computational pipeline

Summarizes total nodes, edges, staleness status, artifact role counts,
and a stage-by-role breakdown table.

## Usage

``` r
pipeline_summary(object)
```

## Arguments

- object:

  A tbl_graph, igraph, or list of FilePath objects.

## Value

Invisibly returns a tibble of the stage breakdown.
