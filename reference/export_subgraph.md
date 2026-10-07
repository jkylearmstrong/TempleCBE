# Export a focused interactive subgraph centered around a focal report or stage

Export a focused interactive subgraph centered around a focal report or
stage

## Usage

``` r
export_subgraph(
  all_objects,
  focal_node = NULL,
  stage = NULL,
  order = 1,
  file = NULL
)
```

## Arguments

- all_objects:

  List of FilePath objects or an igraph object.

- focal_node:

  Character name of the node to focus on (e.g. "EDA_Report").

- stage:

  Character name of the stage to filter by (e.g. "03_Imputation").

- order:

  Degree of neighborhood around focal_node (default 1).

- file:

  Destination HTML path.

## Value

File path invisibly.
