# Add Missing Values Completely at Random

Sets a fixed share of cells in each selected column to `NA`, choosing
the rows independently for each column (missing completely at random).
Use it to test imputation: start from complete data, add missingness,
impute, and compare the imputed cells with the originals.

## Usage

``` r
add_missing(data, cols = dplyr::everything(), pct_na = 0.1)
```

## Arguments

- data:

  A data frame or tibble.

- cols:

  Columns to add missing values to, as a tidyselect expression (default
  [`dplyr::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).

- pct_na:

  Proportion of cells to set to `NA` in each selected column, between 0
  and 1 (default 0.1). Each column gets `round(pct_na * nrow(data))`
  missing cells.

## Value

`data` with missing values added. Its `"missing_cells"` attribute is a
tibble with one row per masked cell, giving its `row` number and
`feature` (column name). Most dplyr verbs drop this attribute, so read
it before transforming the result.

## Details

Every selected column must start with no missing values, so afterwards
its share of `NA`s equals `pct_na` (to the nearest whole row), matching
`PctNa` from
[`features_percent_miss`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md).
The observed share is `1 - pct_na`, so only `pct_na` is given.

## Examples

``` r
set.seed(1)
amputed <- add_missing(mtcars, c(mpg, hp), pct_na = 0.25)
features_percent_miss(amputed)
#> # A tibble: 11 × 5
#>    feature SumNa SumComp PctNa PctComp
#>    <chr>   <int>   <int> <dbl>   <dbl>
#>  1 mpg         8      24  0.25    0.75
#>  2 hp          8      24  0.25    0.75
#>  3 cyl         0      32  0       1   
#>  4 disp        0      32  0       1   
#>  5 drat        0      32  0       1   
#>  6 wt          0      32  0       1   
#>  7 qsec        0      32  0       1   
#>  8 vs          0      32  0       1   
#>  9 am          0      32  0       1   
#> 10 gear        0      32  0       1   
#> 11 carb        0      32  0       1   
head(attr(amputed, "missing_cells"))
#> # A tibble: 6 × 2
#>     row feature
#>   <int> <chr>  
#> 1     1 mpg    
#> 2     2 mpg    
#> 3     4 mpg    
#> 4     7 mpg    
#> 5    11 mpg    
#> 6    14 mpg    
```
