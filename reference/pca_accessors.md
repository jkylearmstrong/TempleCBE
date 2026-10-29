# Scalar Accessors for PCA Results (for Inline Prose)

Small helpers that return one loading, one variance percentage or a
short ranked list from a PCA fit, so a Quarto or R Markdown report can
write “ \`r pca_loading(fit, "age", 1)\` “ instead of keeping local
accessor functions.

## Usage

``` r
pca_loading(pca_mod, feature, pc)

pca_variance_pct(pca_mod, pc, cumulative = FALSE)

pca_top_features(pca_mod, pc, n = 5L, by = c("abs", "positive", "negative"))
```

## Arguments

- pca_mod:

  A \`prcomp\` object or a \[run_pca()\] result.

- feature:

  Name of one original variable (a row name of the rotation matrix). An
  unknown name is an error that lists the available names.

- pc:

  Component number or name (\`"PC2"\`). \`pca_loading()\` takes exactly
  one; \`pca_variance_pct()\` accepts several.

- cumulative:

  If \`TRUE\`, the percentage of variance explained by components 1 to
  \`pc\` together (each value of \`pc\` gives the cumulative share up to
  it).

- n:

  Number of features to return (default 5; fewer if the fit has fewer
  variables).

- by:

  \`"abs"\` (default) ranks by the size of the loading regardless of
  sign; \`"positive"\` and \`"negative"\` rank only the loadings of that
  sign, largest first. Ties keep the order of the variables in the fit.

## Value

\`pca_loading()\`: one number. \`pca_variance_pct()\`: a numeric vector,
one value per \`pc\`. \`pca_top_features()\`: a tibble with \`feature\`,
\`loading\` (signed), \`abs_loading\` and \`rank\`.

## Supported objects

A \[stats::prcomp()\] object, or the result of \[run_pca()\] (which
carries its \`prcomp\` fit). Anything else is an error.
\[stats::princomp()\] fits are not supported.

## Components

\`pc\` is a component number (\`1\`, \`2\`, ...) or a name (\`"PC1"\`).
A component that does not exist in the fit is an error, never \`NA\`.
For \`prcomp(..., rank. = k)\` the variance percentages still use all
components of the full fit (\`sdev\` is complete), so they are shares of
the total variance.

## Scale

Variance percentages are on a 0 to 100 scale (\`12.3\` means 12.3
percent), like \`variance_pct\` in \[run_pca()\]. Loadings are the
elements of \`rotation\`: signed, and unit-length per component. The
sign of a component is arbitrary (a refit can flip it), so describe
direction relative to the other loadings of the same component.

## See also

\[run_pca()\], \[pca_percent_var_explained()\],
\[pca_feature_loading_heatmap()\]

## Examples

``` r
fit <- run_pca(mtcars[, 1:5], scale = TRUE)
pca_loading(fit, "mpg", 1)
#> [1] -0.4624556
pca_variance_pct(fit, 1)
#> [1] 80.75879
pca_variance_pct(fit, 2, cumulative = TRUE)
#> [1] 92.00349
pca_top_features(fit, 1, n = 3)
#> # A tibble: 3 × 4
#>   feature loading abs_loading  rank
#>   <chr>     <dbl>       <dbl> <int>
#> 1 cyl       0.477       0.477     1
#> 2 disp      0.473       0.473     2
#> 3 mpg      -0.462       0.462     3
```
