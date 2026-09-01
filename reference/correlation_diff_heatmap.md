# Heatmap of Correlation Differences Between Two Datasets

Renders the output of
[`correlation_diff`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff.md)
as a heatmap (variable by variable), using the same visual language as
[`pca_loading_diff_heatmap`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff_heatmap.md):
a diverging fill scale centered at zero, so variable pairs with little
change are white and larger correlation changes in either direction
stand out in blue or red.

## Usage

``` r
correlation_diff_heatmap(
  baseline_data,
  comparison_data,
  cor.use = "everything",
  cor.method = "pearson",
  na_omit = TRUE
)
```

## Arguments

- baseline_data:

  A data frame or tibble treated as the reference.

- comparison_data:

  A data frame or tibble to compare against `baseline_data`.

- cor.use, cor.method, na_omit:

  See
  [`correlation_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md).

## Value

A ggplot object.

## Examples

``` r
set.seed(1)
baseline <- mtcars
comparison <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
correlation_diff_heatmap(baseline, comparison)
```
