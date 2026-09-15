# Generate an interactive visNetwork visualization of the computation graph

Generate an interactive visNetwork visualization of the computation
graph

## Usage

``` r
visualize_pipeline_interactive(
  all_objects,
  direction = "LR",
  hierarchical = TRUE,
  title = getOption("pipeline.study_name", "Computational Pipeline"),
  subtitle = "Computational Pipeline"
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

  Character, main title – defaults to getOption("pipeline.study_name",
  "Computational Pipeline").

- subtitle:

  Character, the document-specific name (e.g. "Computational Pipeline",
  or a subgraph's own name from export_subgraph()).

## Value

A visNetwork htmlwidget.
