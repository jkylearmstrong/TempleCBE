# Visualize FilePath dependencies with stage-based coloring and staleness check

Visualize FilePath dependencies with stage-based coloring and staleness
check

## Usage

``` r
visualize_pipeline(
  all_objects,
  extract_graph_code = FALSE,
  stage_colors = NULL,
  orientation = c("TB", "LR", "BT", "RL"),
  font_size = 12,
  node_size = NULL,
  node_sep = 0.5,
  rank_sep = 0.5
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

- orientation:

  Layout direction: \`"TB"\` (Top-to-Bottom, default), \`"LR"\`
  (Left-to-Right), \`"BT"\`, or \`"RL"\`.

- font_size:

  Font size for node labels (default 12).

- node_size:

  Optional numeric vector specifying node dimensions \`c(width,
  height)\`.

- node_sep:

  Minimum space between adjacent nodes in inches (default 0.5).

- rank_sep:

  Separation between successive ranks in inches (default 0.5).
