# Export the computational graph to a self-contained interactive HTML file for stakeholders

Export the computational graph to a self-contained interactive HTML file
for stakeholders

## Usage

``` r
export_interactive_pipeline(
  all_objects,
  file = here::here("R", "workflow_viz", "full_compute_graph.html"),
  direction = c("LR", "UD"),
  hierarchical = TRUE,
  title = NULL,
  subtitle = "Computational Pipeline"
)
```

## Arguments

- all_objects:

  A list of FilePath objects or an igraph object.

- file:

  Path to the output HTML file. Default is here::here("R",
  "workflow_viz", "full_compute_graph.html").

- direction:

  "LR" (default) or "UD".

- hierarchical:

  Logical. Default TRUE.

- title:

  Chart title. Defaults to the study name from
  [`pipeline_config`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_config.md),
  or "Computational Pipeline" if unset.

- subtitle:

  Chart subtitle.

## Value

The saved file path (invisibly).
