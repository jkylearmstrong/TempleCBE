# PCA Biplot

Biplot of feature loading vectors against two principal components.

## Usage

``` r
plot_pca_bi(pca_model, newdata, column, x = 1, y = 2)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

- newdata:

  Data to project onto `pca_model`.

- column:

  Column in `newdata` to use as point labels.

- x, y:

  Which principal components to plot on the x/y axes (default 1, 2).

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
mtcars2 <- tibble::rownames_to_column(mtcars, "model")
plot_pca_bi(pca_model, mtcars2, column = "model")
```
