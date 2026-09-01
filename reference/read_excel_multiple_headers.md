# Read Excel Data With Multi-Row Column Headers

Variant of
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
for sheets where a column's name is split across multiple header rows —
the rows are concatenated in order, joined with `" | "`.

## Usage

``` r
read_excel_multiple_headers(path, n_header_rows, ...)
```

## Arguments

- path:

  Path to the `.xls`/`.xlsx` file.

- n_header_rows:

  Number of header rows in the sheet.

- ...:

  Additional arguments passed to
  [`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html).

## Value

A tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
read_excel_multiple_headers("workbook.xlsx", n_header_rows = 2)
} # }
```
