# Formatted 2x2 Contingency Table with Exact Test

Builds an institutional 2x2 contingency table with row counts, row
percentages, marginal totals, odds ratio estimates, confidence
intervals, and exact test p-values. If any cell count is zero, it
automatically defaults to the mid-p version of Central Fisher's exact
test (`midp = TRUE`).

## Usage

``` r
table_two_by_two(
  data,
  row_var,
  col_var,
  row_label = row_var,
  col_label = col_var,
  midp = NULL,
  conf.level = 0.95
)
```

## Arguments

- data:

  Data frame containing the categorical variables.

- row_var:

  Character name of the row variable.

- col_var:

  Character name of the column variable.

- row_label:

  Optional display label for the row variable.

- col_label:

  Optional display label for the column variable.

- midp:

  Logical or `NULL` (default). If `NULL`, defaults to `TRUE` when any
  cell in the 2x2 table is zero, and `FALSE` otherwise.

- conf.level:

  Confidence level for the odds ratio confidence interval (default
  0.95).

## Value

A list containing:

- table:

  Tibble with formatted cells and marginal totals.

- p_value:

  Exact test p-value.

- estimate:

  Estimated odds ratio.

- conf.int:

  Confidence interval for the odds ratio.

- method:

  Test method name.

- midp:

  Logical indicating whether mid-p adjustment was used.

- has_zero:

  Logical indicating whether any table cell had zero count.

- note:

  Formatted institutional table note.
