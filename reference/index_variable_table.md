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
  method = c("pearson", "spearman"),
  polarities = NULL,
  weights = NULL
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

- polarities:

  Optional \`+1\` / \`-1\` per component, as in
  \[compute_composite_index()\]; adds a \`polarity\` column.

- weights:

  Optional weights per component (in \`cols\` order); adds a \`weight\`
  column.

## Value

A tibble with one row per component variable: \`variable\`, \`polarity\`
and \`weight\` (when given), \`mean\`, \`sd\`, \`min\`, \`max\`,
\`r_with_index\` and \`p_value\`.

## See also

\[compute_composite_index()\]

## Examples

``` r
df <- data.frame(sbp = c(120, 140, 160), dbp = c(80, 90, 100), bmi = c(22, 28, 35))
df <- compute_composite_index(df, cols = c("sbp", "dbp", "bmi"))
index_variable_table(df, cols = c("sbp", "dbp", "bmi"), index_col = "composite_index")
#> # A tibble: 3 × 7
#>   variable  mean    sd   min   max r_with_index p_value
#>   <chr>    <dbl> <dbl> <dbl> <dbl>        <dbl>   <dbl>
#> 1 sbp      140   20      120   160        1.000 0.00942
#> 2 dbp       90   10       80   100        1.000 0.00942
#> 3 bmi       28.3  6.51    22    35        1.000 0.0188 
```
