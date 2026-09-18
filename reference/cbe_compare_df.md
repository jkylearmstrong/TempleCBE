# Compare Two Data Frames (SAS PROC COMPARE Parity)

Compares two data frames (or tibbles) at the dataset, variable, and
observation levels, providing an audit-ready, tidy alternative to SAS
`PROC COMPARE` and legacy comparison utilities (such as
[`arsenal::comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.html)).
Evaluates variable presence, data types, variable labels (from
labelled), row counts, and value-level discrepancies within a
user-specified numerical tolerance.

## Usage

``` r
cbe_compare_df(
  base,
  compare,
  by = NULL,
  tolerance = 1e-07,
  base_name = NULL,
  compare_name = NULL,
  max_diffs = 100
)
```

## Arguments

- base:

  The base data frame or tibble (equivalent to SAS `base=`).

- compare:

  The comparison data frame or tibble (equivalent to SAS `compare=`).

- by:

  Optional character vector of column names specifying key variables
  used to align observations across datasets (equivalent to SAS `id` or
  `by` statement). If `NULL` (default), observations are compared by row
  order.

- tolerance:

  Non-negative numeric threshold for numeric differences (default:
  `1e-7`). Differences with absolute magnitude less than or equal to
  `tolerance` are considered matches.

- base_name:

  Optional character string identifying the base dataset. If `NULL`,
  deparsed from the `base` argument.

- compare_name:

  Optional character string identifying the comparison dataset. If
  `NULL`, deparsed from the `compare` argument.

- max_diffs:

  Maximum number of discrepant rows to store per variable (default:
  100).

## Value

An S3 object of class `"cbe_compare_df"` containing:

- meta:

  List containing metadata on dataset names, dimensions, tolerance, and
  keys.

- variables:

  List with elements `common`, `base_only`, and `compare_only`.

- summary:

  Tibble summarizing comparison status for each variable (types, match
  status, difference counts, max difference, RMSE).

- observations:

  List summarizing observation matching (counts, keys matched,
  unmatched).

- diffs:

  Tibble containing detailed cell-by-cell discrepancies for all
  variables exceeding tolerance.

- is_concordant:

  Logical indicating whether the datasets match completely within
  tolerance on all common variables and observations.

## Examples

``` r
df1 <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c("a", "b", "c", "d", "e"))
df2 <- data.frame(id = 1:5, x = c(1, 2, 3.0001, 4, 5), y = c("a", "b", "c", "d", "f"))
cmp <- cbe_compare_df(df1, df2, by = "id", tolerance = 1e-3)
print(cmp)
#> ---------------------------------------------------------------------- 
#> TempleCBE Data Frame Comparison (SAS PROC COMPARE Parity)
#> ---------------------------------------------------------------------- 
#> Base Data:    df1                       (N = 5, P = 3)
#> Compare Data: df2                       (N = 5, P = 3)
#> By Variables: id
#> Tolerance:    0.001
#> ---------------------------------------------------------------------- 
#> 
#> -- Variable Concordance ----------------------------------------------
#> Variables in Common:       3
#> 
#> -- Observation Concordance -------------------------------------------
#> Matched Observations:      5
#> 
#> -- Discrepancies Summary ---------------------------------------------
#> Variables with Differences: 1 / 2
#> 
#>  variable label type_base type_compare types_match n_diff max_diff rmse
#>         y  <NA> character    character        TRUE      1       NA   NA
#> 
#> Discrepant Values (showing up to 10 rows):
#>  id row_base row_compare variable label base_value compare_value diff
#>   5        5           5        y  <NA>          e             f   NA
#> ---------------------------------------------------------------------- 
generics::tidy(cmp)
#> # A tibble: 1 × 8
#>      id row_base row_compare variable label base_value compare_value  diff
#>   <int>    <int>       <int> <chr>    <chr> <chr>      <chr>         <dbl>
#> 1     5        5           5 y        NA    e          f                NA
```
