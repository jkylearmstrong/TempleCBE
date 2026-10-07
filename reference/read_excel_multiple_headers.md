# Read Excel Data With Multi-Row Column Headers

Variant of
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
for sheets where column names are split across multiple header rows. The
rows are concatenated in order, joined with `sep`. Supports
hierarchical/merged headers via horizontal forward-filling across upper
header tiers, isolates text parsing for headers so data `col_types` do
not collide, and performs robust name repair.

## Usage

``` r
read_excel_multiple_headers(
  path,
  n_header_rows = 2L,
  sheet = 1,
  fill_merged = FALSE,
  sep = " | ",
  clean_names = FALSE,
  trim = TRUE,
  ...
)
```

## Arguments

- path:

  Path to the `.xls`/`.xlsx` file.

- n_header_rows:

  Number of header rows in the sheet (positive integer \>= 1).

- sheet:

  Sheet to read. Either a string (the name of a sheet), or an integer
  (the position of the sheet). Defaults to `1`.

- fill_merged:

  Logical (default `FALSE`); if `TRUE`, handles merged cells in upper
  header rows by forward-filling non-empty values left-to-right across
  columns before vertical concatenation.

- sep:

  Character string used to separate header levels. Default `" | "`.

- clean_names:

  Logical (default `FALSE`); if `TRUE`, applies
  [`clean_names`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_names.md)
  to sanitize the final column names.

- trim:

  Logical (default `TRUE`); whether to trim leading/trailing whitespace
  from header cells.

- ...:

  Additional arguments passed to
  [`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
  for reading the data body (e.g. `col_types`, `na`, `guess_max`).

## Value

A tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2)
read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2, fill_merged = TRUE)
read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2, clean_names = TRUE)
} # }
```
