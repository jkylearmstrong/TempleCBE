# Generate All Pairwise Categorical Combinations

Helper for report generation to enumerate all pairwise combinations of
categorical variables in a dataset with sequential integer IDs and
labels.

## Usage

``` r
cbe_pairwise_combos(data, cols = NULL, labels_df = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- cols:

  Optional character vector of column names to include. If `NULL`, all
  factor and character columns are included.

- labels_df:

  Optional lookup data frame with columns `variable` and `label`.

## Value

A tibble with columns `id`, `var1`, `var2`, `label1`, `label2`, and
`comparison_label`.

## Examples

``` r
df <- data.frame(
  A = factor(c("X", "Y")),
  B = factor(c("1", "2")),
  C = factor(c("M", "N"))
)
cbe_pairwise_combos(df)
#> # A tibble: 3 × 6
#>      id var1  var2  label1 label2 comparison_label
#>   <int> <chr> <chr> <chr>  <chr>  <chr>           
#> 1     1 A     B     A      B      A × B           
#> 2     2 A     C     A      C      A × C           
#> 3     3 B     C     B      C      B × C           
```
