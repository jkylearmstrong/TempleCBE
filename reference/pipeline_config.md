# Get or set global pipeline configuration options

Decouples project-specific parameters (study name, stage display labels,
stage color palettes) from core pipeline mechanics and classes.

## Usage

``` r
pipeline_config(study_name = NULL, stage_labels = NULL, stage_colors = NULL)
```

## Arguments

- study_name:

  Character string for the main study title.

- stage_labels:

  Named character vector mapping raw stage identifiers to human-readable
  labels.

- stage_colors:

  Named list or character vector mapping stage identifiers to hex
  colors.

## Value

A list containing the current configuration options.
