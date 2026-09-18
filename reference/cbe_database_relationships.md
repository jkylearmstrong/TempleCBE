# Analyze Relationships and Linkages Across Datasets in an R Database

Evaluates common key variables (e.g., subject identifiers, visit times)
across multiple datasets in an R database list. Computes cross-table
overlap, record coverage, key cardinality (1:1, 1:many, many:many), and
identifies orphan records.

## Usage

``` r
cbe_database_relationships(db, id_cols = NULL)

# S3 method for class 'cbe_database_relationships'
as_tbl_graph(x, ...)

cbe_find_shared_keys(db)

cbe_check_key_integrity(
  db,
  id_col,
  master_dataset = NULL,
  compare = FALSE,
  tolerance = 1e-07,
  ...
)
```

## Arguments

- db:

  A named list of data frames (an R database).

- id_cols:

  Optional character vector of key/ID column names to evaluate. If
  `NULL`, shared keys are automatically detected across datasets.

- x:

  A `cbe_database_relationships` object.

- ...:

  Additional arguments passed to
  [`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md).

- id_col:

  Character string specifying the identifier column to check.

- master_dataset:

  Character string naming the primary cohort table. If provided, checks
  which IDs in other tables are absent from the master cohort table.

- compare:

  Logical; if `TRUE` and `master_dataset` is supplied, runs
  [`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md)
  on overlapping variables between the master dataset and child
  datasets, attaching the comparison results.

- tolerance:

  Numeric comparison tolerance passed to
  [`cbe_compare_df`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_compare_df.md).

## Value

A tibble summarizing dataset pairs, shared key columns, record counts,
overlap counts, coverage percentages, and cardinality.

## Examples

``` r
db <- list(
  patients = data.frame(id = 1:5, age = c(20, 30, 40, 50, 60)),
  vitals = data.frame(id = c(1, 1, 2, 2, 3, 6), bp = c(120, 122, 130, 132, 110, 115))
)
cbe_database_relationships(db)
#> # A tibble: 1 × 11
#>   from_table to_table key_column from_rows to_rows from_unique_keys
#>   <chr>      <chr>    <chr>          <int>   <int>            <int>
#> 1 patients   vitals   id                 5       6                5
#> # ℹ 5 more variables: to_unique_keys <int>, shared_keys <int>,
#> #   from_coverage_pct <dbl>, to_coverage_pct <dbl>, cardinality <chr>
```
