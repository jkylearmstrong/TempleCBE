# PCA Feature-Loading Heatmap

Heatmap of each original feature's loading onto each principal
component.

## Usage

``` r
pca_feature_loading_heatmap(pca_model)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object or `cbe_pca`
  object from
  [`run_pca`](https://jkylearmstrong.github.io/TempleCBE/reference/run_pca.md).

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_feature_loading_heatmap(pca_model)
```
