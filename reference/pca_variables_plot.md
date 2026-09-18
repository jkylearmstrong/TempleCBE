# PCA Variable Correlation Circle

Plots the projection of variables onto the unit circle for two principal
components, showing correlations between original variables and
principal components.

## Usage

``` r
pca_variables_plot(
  pca_model,
  x = 1,
  y = 2,
  title = "Variables Correlation Circle"
)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object or output
  from
  [`proc_pca`](https://jkylearmstrong.github.io/TempleCBE/reference/proc_pca.md).

- x, y:

  Which principal components to plot (default 1, 2).

- title:

  Optional plot title (default `"Variables Correlation Circle"`).

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
pca_variables_plot(pca_model)
```
