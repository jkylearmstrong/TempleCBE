# Difference in PCA Loadings Between Two Fits

Compares the variable loadings of two independently-fit
[`prcomp`](https://rdrr.io/r/stats/prcomp.html) objects on the same set
of variables (e.g. the same domain's data fit at baseline vs. at a later
timepoint), matching components positionally (both fits' PC1, both fits'
PC2, ...).

## Usage

``` r
pca_loading_diff(pca_baseline, pca_comparison, n_components = NULL)
```

## Arguments

- pca_baseline:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object treated as
  the reference.

- pca_comparison:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object to compare
  against `pca_baseline`, fit on the same (or overlapping) variables.

- n_components:

  Number of leading components to compare. Defaults to `NULL`, meaning
  all components shared by both fits.

## Value

A tibble with one row per shared variable (`feature` column) and one
column per compared component (`PC1`, `PC2`, ...) holding the
sign-aligned difference (comparison minus baseline).

## Details

PCA loading vectors are only unique up to sign: a component can flip
orientation between two otherwise-equivalent fits without changing the
pattern it represents. Naively differencing loadings would then show a
spuriously large change (up to roughly double the loading) for a
component that hasn't meaningfully changed at all. To avoid this, for
each shared component `pca_comparison`'s loading vector is sign- aligned
to `pca_baseline`'s: it is flipped (multiplied by -1) if doing so
reduces the total absolute difference across variables relative to
leaving it as-is. The difference is then computed as (sign-aligned
comparison) minus baseline.

Variables are matched by name (the rownames of `$rotation`). If the two
fits were built on different variable sets, only the intersection is
used; no error is raised.

## Examples

``` r
set.seed(1)
baseline <- prcomp(mtcars, center = TRUE, scale. = TRUE)
comparison <- prcomp(mtcars[sample(nrow(mtcars)), ], center = TRUE, scale. = TRUE)
pca_loading_diff(baseline, comparison)
#> # A tibble: 11 × 12
#>    feature       PC1       PC2       PC3       PC4       PC5       PC6       PC7
#>    <chr>       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 mpg      0        -1.94e-16 -4.72e-16 -3.40e-16  1.94e-16  2.50e-16  1.00e-14
#>  2 cyl      1.11e-16 -1.11e-16 -6.38e-16 -5.00e-16  4.37e-16  1.11e-16  4.77e-15
#>  3 disp     1.11e-16  1.11e-16 -1.39e-17 -1.33e-15 -2.22e-16  1.28e-15  0       
#>  4 hp       5.55e-17  1.94e-16  1.67e-16 -1.67e-16 -1.11e-16  1.67e-15  2.30e-15
#>  5 drat    -5.55e-17  5.55e-17 -1.67e-16 -3.33e-16 -9.71e-17 -3.89e-16  6.25e-17
#>  6 wt       1.11e-16  5.55e-17  5.55e-17 -3.89e-16 -1.15e-15 -9.44e-16  3.19e-16
#>  7 qsec    -2.78e-17  1.11e-16  2.22e-16 -6.94e-16 -8.33e-16 -8.88e-16  2.69e-15
#>  8 vs      -5.55e-17  5.55e-17  0        -1.67e-16  7.77e-16  2.44e-15  9.44e-16
#>  9 am       0         0         2.22e-16 -7.49e-16 -1.54e-15  4.44e-16  4.44e-16
#> 10 gear    -2.78e-17  1.11e-16 -3.33e-16 -4.44e-16  4.44e-16 -2.50e-16 -4.00e-15
#> 11 carb     2.78e-17  2.22e-16 -4.44e-16  2.78e-16  7.77e-16 -3.33e-16  5.16e-15
#> # ℹ 4 more variables: PC8 <dbl>, PC9 <dbl>, PC10 <dbl>, PC11 <dbl>
```
