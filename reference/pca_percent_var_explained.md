# Percent Variance Explained by Each Principal Component

The percentages come from the component standard deviations
(`pca_model$sdev`), as in
[`pca_scree_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_scree_plot.md),
and are shares of the total variance. A fit truncated with `rank.` or
`tol` still stores every standard deviation, so all components are
shown.

## Usage

``` r
pca_percent_var_explained(pca_model)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

## Value

A ggplot object showing per-component and cumulative variance explained.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_percent_var_explained(pca_model)
```
