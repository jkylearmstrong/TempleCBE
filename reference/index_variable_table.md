# Summary Table of Composite Index Component Variables

Generates a tidy summary table of component variables comprising a
composite index, reporting weights/loadings, correlation with the index,
mean, SD, and p-value.

## Usage

``` r
index_variable_table(
  data,
  cols,
  index_col = "composite_index",
  method = c("pearson", "spearman")
)
```

## Arguments

- data:

  Data frame containing component columns and the composite index
  column.

- cols:

  Character vector of component variable names.

- index_col:

  Character name of the composite index column.

- method:

  Correlation method: \`"pearson"\` (default) or \`"spearman"\`.

## Value

A tibble with one row per component variable.

## See also

\[compute_composite_index()\]

## Examples

``` r
df <- data.frame(sbp = c(120, 140, 160), dbp = c(80, 90, 100), bmi = c(22, 28, 35))
df <- compute_composite_index(df, cols = c("sbp", "dbp", "bmi"))
index_variable_table(df, cols = c("sbp", "dbp", "bmi"), index_col = "composite_index")
#> # A tibble: 3 × 5
#>   variable  mean    sd r_with_index p_value
#>   <chr>    <dbl> <dbl>        <dbl>   <dbl>
#> 1 sbp      140   20           1.000 0.00942
#> 2 dbp       90   10           1.000 0.00942
#> 3 bmi       28.3  6.51        1.000 0.0188 
```
