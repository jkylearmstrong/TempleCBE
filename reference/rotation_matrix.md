# PCA Rotation Matrix (Loadings)

PCA Rotation Matrix (Loadings)

## Usage

``` r
rotation_matrix(PC_mod)

pca_loadings(PC_mod)
```

## Arguments

- PC_mod:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

## Value

A tibble of feature loadings onto each principal component, with a
`feature_num` column (`"f1"`, `"f2"`, ...) for compact labeling in
[`pca_eqns`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_eqns.md).

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
rotation_matrix(pca_model)
#> # A tibble: 11 × 13
#>    feature    PC1     PC2     PC3      PC4     PC5     PC6      PC7      PC8
#>    <chr>    <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
#>  1 mpg     -0.363  0.0161 -0.226  -0.0225  -0.103  -0.109   0.368    0.754  
#>  2 cyl      0.374  0.0437 -0.175  -0.00259 -0.0585  0.169   0.0573   0.231  
#>  3 disp     0.368 -0.0493 -0.0615  0.257   -0.394  -0.336   0.214   -0.00114
#>  4 hp       0.330  0.249   0.140  -0.0677  -0.540   0.0714 -0.00150  0.222  
#>  5 drat    -0.294  0.275   0.161   0.855   -0.0773  0.244   0.0211  -0.0322 
#>  6 wt       0.346 -0.143   0.342   0.246    0.0750 -0.465  -0.0207   0.00857
#>  7 qsec    -0.200 -0.463   0.403   0.0681   0.165  -0.330   0.0500   0.232  
#>  8 vs      -0.307 -0.232   0.429  -0.215   -0.600   0.194  -0.266   -0.0259 
#>  9 am      -0.235  0.429  -0.206  -0.0305  -0.0898 -0.571  -0.587    0.0597 
#> 10 gear    -0.207  0.462   0.290  -0.265   -0.0483 -0.244   0.605   -0.336  
#> 11 carb     0.214  0.414   0.529  -0.127    0.361   0.184  -0.175    0.396  
#> # ℹ 4 more variables: PC9 <dbl>, PC10 <dbl>, PC11 <dbl>, feature_num <chr>
```
