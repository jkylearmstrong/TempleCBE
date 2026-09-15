# Convert to an enriched igraph graph with rich metadata

Convert to an enriched igraph graph with rich metadata

## Usage

``` r
as_igraph(all_objects, stage_colors = NULL)
```

## Arguments

- all_objects:

  list of FilePath, FileUses, and FileOutputs objects.

- stage_colors:

  Optional named list of stage colors. If NULL, uses pipeline_config()
  options.

## Value

an igraph object containing complete node and edge attributes
