# Compare Two Data Frames (Modelled on SAS PROC COMPARE)

Compares two data frames (or tibbles) at the dataset, variable, and
observation levels, providing an audit-ready, tidy alternative to SAS
`PROC COMPARE` and legacy comparison utilities (such as
[`arsenal::comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.html)).
Evaluates variable presence, data types, variable labels (from
labelled), row counts, and value-level discrepancies within a
user-specified numerical tolerance. It agrees with `PROC COMPARE` on the
benchmark described under “Agreement with SAS PROC COMPARE”, and differs
from it in the ways listed there.

## Usage

``` r
cbe_compare_df(base, compare, ...)

# Default S3 method
cbe_compare_df(
  base,
  compare,
  by = NULL,
  tolerance = 1e-07,
  base_name = NULL,
  compare_name = NULL,
  max_diffs = 100,
  ...
)

# S3 method for class 'cbe_database'
cbe_compare_df(
  base,
  compare,
  by = NULL,
  edge_by = c("from", "to"),
  ...,
  base_name = NULL,
  compare_name = NULL
)

# S3 method for class 'igraph'
cbe_compare_df(base, compare, ...)

# S3 method for class 'visNetwork'
cbe_compare_df(base, compare, ...)

compare_df(base, compare, ...)
```

## Arguments

- base:

  The base data frame or tibble (equivalent to SAS `base=`).

- compare:

  The comparison data frame or tibble (equivalent to SAS `compare=`).

- ...:

  Passed on to methods (and, for the `cbe_database` method, on to the
  per-table `cbe_compare_df()` calls – e.g. `by`/`tolerance`).

- by:

  Optional character vector of column names specifying key variables
  used to align observations across datasets (equivalent to SAS `id` or
  `by` statement). If `NULL` (default), observations are compared by row
  order. Rows that share a key are paired in order of appearance (with a
  warning); surplus duplicates on either side count as unmatched. A key
  column named like one of the `diffs` columns (`variable`, `label`,
  ...) is prefixed with `key_` there. For the `cbe_database` method,
  this is the node key (passed through to the nodes comparison only).

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

- edge_by:

  `cbe_database` method only: key column(s) used to align edges (default
  `c("from", "to")`, which every
  [`as_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)
  method produces).

## Value

An S3 object of class `"cbe_compare_df"` containing (or, for a
[`cbe_database`](https://jkylearmstrong.github.io/TempleCBE/reference/as_database.md)/graph
comparison, a `"cbe_compare_database"` list of two such objects, named
`nodes` and `edges`):

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

## Agreement with SAS PROC COMPARE

The package tests compare the results with SAS 9.4 `PROC COMPARE`
(`METHOD=ABSOLUTE`, `CRITERION` equal to `tolerance`, `by` as the `ID`
statement) on nine small comparisons typed into
`inst/sas/benchmark_proc_compare.sas`; its listing is committed under
`inst/sas/list/`. On that data both agree on the number of variables in
common and only in either data set; of observations matched and only in
either data set (by key, by position, and with repeated keys paired in
order); of values that differ, per variable and in all, and of
observations with a difference; of values that are not exactly equal
(`tolerance = 0`); on the maximum difference; and on every cell that
differs. A value differs if `abs(difference) > tolerance` (a difference
of exactly `tolerance` is none), and a missing value against a value is
a difference. The agreement is on the variables SAS compares (see the
second item below) and on that data; it is not a proof for other inputs.

The two define some things differently:

- The sign: `diff` in `diffs` is base minus compare; `PROC COMPARE`
  prints `Diff.` as compare minus base.

- A variable whose type differs between the data sets is still compared,
  as text, and reported with `types_match = FALSE`; `PROC COMPARE`
  compares nothing and counts a conflicting type.

- “Type” is the R class: `integer` against `numeric`, or `factor`
  against `character`, is a type mismatch, although SAS has only numeric
  and character variables.

- Strings are compared as they are: a trailing blank is a difference
  (SAS pads the shorter value), and `NA` differs from `""` (SAS's
  missing character value is the blank).

- Only the type is checked, not the length, format, informat or label
  (SAS reports a character variable of different lengths as a differing
  attribute).

- Only absolute differences. `METHOD=PERCENT` and `RELATIVE`, `BY` group
  reports and special missing values are not part of the benchmark.

- `cbe_compare_df()` needs no sorted data; `PROC COMPARE` does (the
  benchmark sorts the COMPARE data set).

## Examples

``` r
df1 <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c("a", "b", "c", "d", "e"))
df2 <- data.frame(id = 1:5, x = c(1, 2, 3.0001, 4, 5), y = c("a", "b", "c", "d", "f"))
cmp <- cbe_compare_df(df1, df2, by = "id", tolerance = 1e-3)
print(cmp)
#> ---------------------------------------------------------------------- 
#> TempleCBE Data Frame Comparison (modelled on SAS PROC COMPARE)
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
g1 <- igraph::graph_from_data_frame(
  data.frame(from = c("a", "b"), to = c("b", "c")),
  vertices = data.frame(name = c("a", "b", "c"), stage = c("eda", "eda", "report"))
)
g2 <- igraph::graph_from_data_frame(
  data.frame(from = c("a", "b"), to = c("b", "c")),
  vertices = data.frame(name = c("a", "b", "c"), stage = c("eda", "analysis", "report"))
)
cbe_compare_df(g1, g2, by = "name")
#> == Nodes ===============================================================
#> ---------------------------------------------------------------------- 
#> TempleCBE Data Frame Comparison (modelled on SAS PROC COMPARE)
#> ---------------------------------------------------------------------- 
#> Base Data:    as_database(base)$nodes   (N = 3, P = 2)
#> Compare Data: as_database(compare)$nodes (N = 3, P = 2)
#> By Variables: name
#> Tolerance:    1e-07
#> ---------------------------------------------------------------------- 
#> 
#> -- Variable Concordance ----------------------------------------------
#> Variables in Common:       2
#> 
#> -- Observation Concordance -------------------------------------------
#> Matched Observations:      3
#> 
#> -- Discrepancies Summary ---------------------------------------------
#> Variables with Differences: 1 / 1
#> 
#>  variable label type_base type_compare types_match n_diff max_diff rmse
#>     stage  <NA> character    character        TRUE      1       NA   NA
#> 
#> Discrepant Values (showing up to 10 rows):
#>  name row_base row_compare variable label base_value compare_value diff
#>     b        2           2    stage  <NA>        eda      analysis   NA
#> ---------------------------------------------------------------------- 
#> 
#> == Edges ===============================================================
#> ---------------------------------------------------------------------- 
#> TempleCBE Data Frame Comparison (modelled on SAS PROC COMPARE)
#> ---------------------------------------------------------------------- 
#> Base Data:    as_database(base)$edges   (N = 2, P = 2)
#> Compare Data: as_database(compare)$edges (N = 2, P = 2)
#> By Variables: from, to
#> Tolerance:    1e-07
#> ---------------------------------------------------------------------- 
#> 
#> -- Variable Concordance ----------------------------------------------
#> Variables in Common:       2
#> 
#> -- Observation Concordance -------------------------------------------
#> Matched Observations:      2
#> 
#> -- Discrepancies Summary ---------------------------------------------
#> Result: All values match within tolerance 1e-07.
#> Status: Data sets are completely CONCORDANT.
#> ---------------------------------------------------------------------- 
```
