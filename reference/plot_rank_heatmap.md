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
  low_color = "#9D2235",
  high_color = "#F4F4F4",
  title = "Rank Stability Across Resamples",
  base_size = 12
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

  Color for best rank (rank 1, default Temple Cherry \`"#9D2235"\`).

- high_color:

  Color for worst rank (default light grey \`"#F4F4F4"\`).

- title:

  Optional plot title.

- base_size:

  Base font size (default 12).

## Value

A \`ggplot2\` object.
