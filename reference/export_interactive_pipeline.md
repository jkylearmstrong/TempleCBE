# Export the computational graph to a self-contained interactive HTML file for stakeholders

Export the computational graph to a self-contained interactive HTML file
for stakeholders

## Usage

``` r
export_interactive_pipeline(
  all_objects,
  file = here::here("R", "workflow_viz", "full_compute_graph.html"),
  direction = "LR",
  hierarchical = TRUE,
  title = getOption("pipeline.study_name", "Computational Pipeline"),
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

## Value

The saved file path (invisibly).
