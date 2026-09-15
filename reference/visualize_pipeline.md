# Visualize FilePath dependencies with stage-based coloring and staleness check

Visualize FilePath dependencies with stage-based coloring and staleness
check

## Usage

``` r
visualize_pipeline(
  all_objects,
  extract_graph_code = FALSE,
  stage_colors = NULL
)
```

## Arguments

- all_objects:

  The full list of FilePath, FileUses, and FileOutputs objects.

- extract_graph_code:

  Logical. If TRUE, the graph source code will be extracted and returned
  as a character string.

- stage_colors:

  Optional named list of stage colors. If NULL, uses pipeline_config()
  options.
