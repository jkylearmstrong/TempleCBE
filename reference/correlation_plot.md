# Correlation Plot

Plots a correlation matrix across all numeric columns of `data` using
corrplot.

## Usage

``` r
correlation_plot(
  data,
  cor.use = "everything",
  cor.method = "pearson",
  method = "ellipse",
  type = "upper",
  order = "FPC",
  title = "Correlation Coefficient Plot",
  na_omit = TRUE,
  tl.cex = 0.5,
  number.cex = 0.75,
  tl.srt = 45,
  ...
)
```

## Arguments

- data:

  A data frame or tibble.

- cor.use:

  Passed to [`cor`](https://rdrr.io/r/stats/cor.html): `"everything"`
  (default), `"all.obs"`, `"complete.obs"`, `"na.or.complete"`, or
  `"pairwise.complete.obs"`.

- cor.method:

  Passed to [`cor`](https://rdrr.io/r/stats/cor.html): `"pearson"`
  (default), `"kendall"`, or `"spearman"`.

- method:

  Visualization method passed to
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html) (default
  `"ellipse"`).

- type:

  `"upper"` (default), `"full"`, or `"lower"`.

- order:

  Ordering method for the correlation matrix (default `"FPC"`, first
  principal component order).

- title:

  Plot title.

- na_omit:

  Logical (default `TRUE`); drop rows with any `NA` among the numeric
  columns before computing correlations.

- tl.cex, number.cex, tl.srt:

  Label/number sizing and rotation, passed to
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html).

- ...:

  Additional arguments passed to
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html).

## Value

Invisibly, the correlation matrix (called for its plot side effect).

## Examples

``` r
correlation_plot(mtcars, tl.cex = .7)
```
