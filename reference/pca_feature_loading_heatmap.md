# PCA Feature-Loading Heatmap

Heatmap of each original feature's loading onto each principal
component.

## Usage

``` r
pca_feature_loading_heatmap(pca_model)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_feature_loading_heatmap(pca_model)
```
