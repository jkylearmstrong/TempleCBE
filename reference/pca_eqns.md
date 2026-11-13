# PCA Equations

Writes out each principal component as a linear equation in the original
features (abbreviated `f1`, `f2`, ... — see the returned `labels` table
for what each abbreviation means).

## Usage

``` r
pca_eqns(PC_mod, precision = 3)
```

## Arguments

- PC_mod:

  A [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

- precision:

  Digits to round loadings to (default 3).

## Value

A list with two tibbles: `eqns` (one row per component, with its
equation as text) and `labels` (feature-number-to-name key).

## Examples

``` r
pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
pca_eqns(pca_model)$eqns
#> # A tibble: 11 × 2
#>    PC       rhs                                                                 
#>    <chr>    <chr>                                                               
#>  1 "PC1= "  -0.363*(f1) +0.374*(f2) +0.368*(f3) +0.33*(f4) -0.294*(f5) +0.346*(…
#>  2 "PC2= "  +0.016*(f1) +0.044*(f2) -0.049*(f3) +0.249*(f4) +0.275*(f5) -0.143*…
#>  3 "PC3= "  -0.226*(f1) -0.175*(f2) -0.061*(f3) +0.14*(f4) +0.161*(f5) +0.342*(…
#>  4 "PC4= "  -0.023*(f1) -0.003*(f2) +0.257*(f3) -0.068*(f4) +0.855*(f5) +0.246*…
#>  5 "PC5= "  -0.103*(f1) -0.058*(f2) -0.394*(f3) -0.54*(f4) -0.077*(f5) +0.075*(…
#>  6 "PC6= "  -0.109*(f1) +0.169*(f2) -0.336*(f3) +0.071*(f4) +0.244*(f5) -0.465*…
#>  7 "PC7= "  +0.368*(f1) +0.057*(f2) +0.214*(f3) -0.001*(f4) +0.021*(f5) -0.021*…
#>  8 "PC8= "  +0.754*(f1) +0.231*(f2) -0.001*(f3) +0.222*(f4) -0.032*(f5) +0.009*…
#>  9 "PC9= "  -0.236*(f1) -0.054*(f2) -0.198*(f3) +0.576*(f4) +0.047*(f5) -0.359*…
#> 10 "PC10= " -0.139*(f1) +0.846*(f2) -0.049*(f3) -0.248*(f4) +0.101*(f5) -0.094*…
#> 11 "PC11= " -0.125*(f1) -0.141*(f2) +0.661*(f3) -0.256*(f4) -0.04*(f5) -0.567*(…
pca_eqns(pca_model)$labels
#> # A tibble: 11 × 2
#>    fi       feature
#>    <chr>    <chr>  
#>  1 "f1 = "  mpg    
#>  2 "f2 = "  cyl    
#>  3 "f3 = "  disp   
#>  4 "f4 = "  hp     
#>  5 "f5 = "  drat   
#>  6 "f6 = "  wt     
#>  7 "f7 = "  qsec   
#>  8 "f8 = "  vs     
#>  9 "f9 = "  am     
#> 10 "f10 = " gear   
#> 11 "f11 = " carb   
```
