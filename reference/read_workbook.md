# Read Every Sheet of an Excel Workbook

Variant of
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
that reads every sheet in a workbook into a named list of tibbles, named
after each sheet.

## Usage

``` r
read_workbook(path, ...)
```

## Arguments

- path:

  Path to the `.xls`/`.xlsx` file.

- ...:

  Additional arguments passed to
  [`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html).

## Value

A named list of tibbles, one per sheet.

## Examples

``` r
if (FALSE) { # \dontrun{
read_workbook("workbook.xlsx")
} # }
```
