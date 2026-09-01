# Missingness Map

Visualizes missingness across a data frame as a heatmap, either per
row/column or aggregated by a grouping column.

## Usage

``` r
missmap(df, by_column = NULL, na_list = NULL, row_order = FALSE)
```

## Arguments

- df:

  A data frame or tibble.

- by_column:

  Optional unquoted column name to aggregate missingness by (e.g. a site
  or subject id) instead of plotting every row individually.

- na_list:

  Optional vector of additional values to treat as missing (beyond
  actual `NA`).

- row_order:

  Logical; if `FALSE` (default) rows/features are ordered by how much
  missingness they have.

## Value

A ggplot object.

## Examples

``` r
df <- data.frame(
  a = c(1, NA, 3, 4, NA),
  b = c(NA, NA, 3, 4, 5),
  site = c("A", "A", "B", "B", "B")
)
missmap(df)

missmap(df, by_column = site)
```
