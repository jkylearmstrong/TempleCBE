# Process and Plot Principal Component Analysis (PCA)

Extracts a per-component variance summary from a PCA fit. Accepts either
an existing [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object or
raw data, in which case [`prcomp`](https://rdrr.io/r/stats/prcomp.html)
is run first.

## Usage

``` r
proc_pca(data, center = TRUE, scale = TRUE, ...)
```

## Arguments

- data:

  A \`prcomp\` object, or a numeric matrix/data frame to fit PCA on.

- center, scale:

  Passed to [`prcomp`](https://rdrr.io/r/stats/prcomp.html) (as
  \`center\` and \`scale.\`) when \`data\` is raw data; ignored when
  \`data\` is already a \`prcomp\` object.

- ...:

  Further arguments passed to
  [`prcomp`](https://rdrr.io/r/stats/prcomp.html) when \`data\` is raw
  data.

## Value

A tibble with one row per component: \`component\`, \`eigenvalue\`,
\`variance_pct\`, \`cum_variance_pct\`.

## Examples

``` r
proc_pca(prcomp(mtcars[, 1:4], scale. = TRUE))
#> # A tibble: 4 × 4
#>   component eigenvalue variance_pct cum_variance_pct
#>   <chr>          <dbl>        <dbl>            <dbl>
#> 1 PC1           3.50          87.6              87.6
#> 2 PC2           0.239          5.99             93.5
#> 3 PC3           0.165          4.13             97.7
#> 4 PC4           0.0931         2.33            100  

# Or fit the PCA in one step
proc_pca(mtcars[, 1:4], scale = TRUE)
#> # A tibble: 4 × 4
#>   component eigenvalue variance_pct cum_variance_pct
#>   <chr>          <dbl>        <dbl>            <dbl>
#> 1 PC1           3.50          87.6              87.6
#> 2 PC2           0.239          5.99             93.5
#> 3 PC3           0.165          4.13             97.7
#> 4 PC4           0.0931         2.33            100  
```
