# Heatmap of Variable or Model Ranks Across Resamples or Metrics

Generates a tile heatmap displaying ranks of features or candidate
models across cross-validation resamples, metrics, or methods. Replaces
inline copies across reports.

## Usage

``` r
plot_rank_heatmap(
  data,
  item_col,
  group_col,
  rank_col,
  low_color = "#A41E35",
  high_color = "#F4F4F4",
  title = "Rank Stability Across Resamples",
  base_size = 12,
  fill_col = NULL,
  palette = NULL
)
```

## Arguments

- data:

  A data frame containing item names, resampling split/method
  identifiers, and ranks.

- item_col:

  Character name of feature or model column (y-axis).

- group_col:

  Character name of fold, split, or metric column (x-axis).

- rank_col:

  Character name of rank column (fill).

- low_color:

  Color for best rank (rank 1, default Temple Cherry \`"#A41E35"\`).

- high_color:

  Color for worst rank (default light grey \`"#F4F4F4"\`).

- title:

  Optional plot title.

- base_size:

  Base font size (default 12).

- fill_col:

  Optional name of a numeric column to color the tiles by instead of the
  rank, for example the mean coefficient estimate. The fill is then a
  diverging scale centered on 0 (blue below, white at 0, \`low_color\`
  above) and the rank is still the text in each tile.

- palette:

  Optional name of a sequential ColorBrewer palette (for example
  \`"Blues"\`) for the rank fill, with the best rank darkest; used
  instead of \`low_color\` / \`high_color\`. Ignored with \`fill_col\`.

## Value

A \`ggplot2\` object.
