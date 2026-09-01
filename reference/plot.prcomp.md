# Generic Plot Method for `prcomp` Objects

Generic Plot Method for `prcomp` Objects

## Usage

``` r
# S3 method for class 'prcomp'
plot(x, type = c("variance", "heatmap", "bi", "biplot"), ...)
```

## Arguments

- x:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

- type:

  One of `"variance"`
  ([`pca_percent_var_explained`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_percent_var_explained.md)),
  `"heatmap"`
  ([`pca_feature_loading_heatmap`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_feature_loading_heatmap.md)),
  `"bi"`
  ([`plot_pca_bi`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md)),
  or `"biplot"`
  ([`pca_biplot`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_biplot.md)).

- ...:

  Passed on to the underlying plot function (needed for `type = "bi"`,
  which requires `newdata` and `column`; and optionally used by
  `type = "biplot"` to pass `x`/`y`).

## Value

A ggplot object.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
plot(pca_model, type = "variance")
```
