# Generate an interactive visNetwork visualization of the computation graph

Generate an interactive visNetwork visualization of the computation
graph

## Usage

``` r
visualize_pipeline_interactive(
  all_objects,
  direction = c("LR", "UD"),
  hierarchical = TRUE,
  title = NULL,
  subtitle = "Computational Pipeline",
  font_size = 12,
  node_size = 25
)
```

## Arguments

- all_objects:

  A list of FilePath objects or an igraph object.

- direction:

  Layout direction: "LR" (Left-to-Right, default) or "UD"
  (Top-to-Bottom).

- hierarchical:

  Logical, whether to use a hierarchical DAG layout. Default is TRUE.

- title:

  Character, main title – defaults to the study name from
  [`pipeline_config`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_config.md).

- subtitle:

  Character, the document-specific name (e.g. "Computational Pipeline",
  or a subgraph's own name from export_subgraph()).

- font_size:

  Numeric font size for node labels (default 12).

- node_size:

  Numeric node size in pixels (default 25).

## Value

A visNetwork htmlwidget.
