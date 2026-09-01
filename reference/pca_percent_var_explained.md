# Percent Variance Explained by Each Principal Component

Percent Variance Explained by Each Principal Component

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
