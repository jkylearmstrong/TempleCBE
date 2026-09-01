# PCA Loadings Biplot

A classic PCA biplot: the observation scores (`pca_model$x`) as a muted
point cloud, overlaid with the variable loading vectors
(`pca_model$rotation`) drawn as labeled arrows from the origin. Unlike
[`plot_pca_bi`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md),
no separate `newdata` is required – a fitted
[`prcomp`](https://rdrr.io/r/stats/prcomp.html) object already carries
both the scores and the loadings needed to draw the biplot.

## Usage

``` r
pca_biplot(pca_model, x = 1, y = 2)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

- x, y:

  Which principal components to plot on the x/y axes (default 1, 2).

## Value

A ggplot object.

## Details

Loading vectors are unit-scale by construction and would be invisible
next to the score cloud if plotted as-is, so they are rescaled so that
their maximum extent is 80 standard biplot convention) before being
drawn.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_biplot(pca_model, x = 1, y = 2)
```
