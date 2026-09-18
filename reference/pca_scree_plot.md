# PCA Scree Plot

Scree plot displaying the eigenvalues or proportion of variance
explained by each principal component, with an optional horizontal
reference line at eigenvalue = 1 (Kaiser-Guttman criterion).

## Usage

``` r
pca_scree_plot(
  pca_model,
  metric = c("eigenvalue", "variance"),
  kaiser = TRUE,
  title = "PCA Scree Plot"
)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object or output
  from
  [`proc_pca`](https://jkylearmstrong.github.io/TempleCBE/reference/proc_pca.md).

- metric:

  Which metric to plot: `"eigenvalue"` (default, with Kaiser line at 1)
  or `"variance"` (percent of variance explained).

- kaiser:

  Logical (default `TRUE`); draw horizontal dashed line at eigenvalue =
  1.

- title:

  Optional plot title.

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
pca_scree_plot(pca_model)
```
