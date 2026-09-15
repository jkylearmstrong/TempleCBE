# Plot a PCA Fit

One entry point to this package's PCA plots.

## Usage

``` r
pca_plot(pca_model, type = c("variance", "heatmap", "bi", "biplot"), ...)
```

## Arguments

- pca_model:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object. (Not \`x\`,
  which would capture the \`x\` component argument of \`type = "bi"\`
  and \`"biplot"\`.)

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
  `type = "bi"` or `"biplot"` to pick components with `x`/`y`).

## Value

A ggplot object.

## Details

This is an ordinary function rather than a \`plot()\` method: stats
already registers \`plot()\` for \`prcomp\` objects (a scree plot), and
a package that replaces another package's method for a class it doesn't
own changes \`plot()\` for everyone who loads it. Call \`pca_plot(x,
type = )\`, or \[stats::screeplot()\] for the base scree plot.

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_plot(pca_model, type = "variance")

pca_plot(pca_model, type = "biplot", x = 1, y = 3)
```
