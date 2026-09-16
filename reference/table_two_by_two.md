# Formatted 2x2 Contingency Table with Fisher's Exact Test

Builds an institutional 2x2 contingency table with row counts, row
percentages, marginal totals, and Fisher's exact test p-value.

## Usage

``` r
table_two_by_two(
  data,
  row_var,
  col_var,
  row_label = row_var,
  col_label = col_var
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

## Value

A list containing `table` (tibble) and `note` (character).
