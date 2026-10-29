# Distribution of Clinical Strata Across Quadrants

Counts of each level of the stratifying variables within each quadrant.

## Usage

``` r
tbl_quadrant_distribution(
  data,
  quadrant_col = "quadrant",
  strat_vars,
  include_total = TRUE
)
```

## Arguments

- data:

  A data frame with a quadrant column (see \[classify_quadrants()\]).

- quadrant_col:

  Name of the quadrant column.

- strat_vars:

  Names of the categorical (or logical) variables to count.

- include_total:

  Add a \`Total\` pseudo-variable with the number of observations per
  quadrant.

## Value

A long tibble with \`quadrant\`, \`variable\`, \`level\`, \`n\` and
\`pct\` (percent of the quadrant's observations with a non-missing
quadrant; missing values of a stratifying variable count in the
denominator but have no row). Rows with a missing quadrant are dropped.

## See also

\[classify_quadrants()\], \[quadrant_n()\]

## Examples

``` r
d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
tbl_quadrant_distribution(d, strat_vars = c("am", "vs"))
#> # A tibble: 20 × 5
#>    quadrant variable level     n    pct
#>    <fct>    <chr>    <chr> <int>  <dbl>
#>  1 I        am       0         1 100   
#>  2 I        am       1         0   0   
#>  3 I        vs       0         0   0   
#>  4 I        vs       1         1 100   
#>  5 I        Total    Total     1 100   
#>  6 II       am       0         3  23.1 
#>  7 II       am       1        10  76.9 
#>  8 II       vs       0         3  23.1 
#>  9 II       vs       1        10  76.9 
#> 10 II       Total    Total    13 100   
#> 11 III      am       0         0   0   
#> 12 III      am       1         2 100   
#> 13 III      vs       0         2 100   
#> 14 III      vs       1         0   0   
#> 15 III      Total    Total     2 100   
#> 16 IV       am       0        15  93.8 
#> 17 IV       am       1         1   6.25
#> 18 IV       vs       0        13  81.2 
#> 19 IV       vs       1         3  18.8 
#> 20 IV       Total    Total    16 100   
```
