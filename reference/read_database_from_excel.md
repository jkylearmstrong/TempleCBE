# Read an R Database from an Excel Workbook

Reads all worksheets from an Excel workbook into a named list of
tibbles, optionally extracting and attaching the `METADATA` sheet as an
attribute.

## Usage

``` r
read_database_from_excel(
  path,
  include_metadata = TRUE,
  metadata_sheet = "METADATA",
  ...
)

cbe_read_database(
  path,
  include_metadata = TRUE,
  metadata_sheet = "METADATA",
  ...
)
```

## Arguments

- path:

  Path to the `.xls`/`.xlsx` file.

- include_metadata:

  Logical; if `TRUE` (default) and a sheet named `metadata_sheet` is
  found, it is separated and attached as attribute `"metadata"` on the
  returned database list.

- metadata_sheet:

  Name of the metadata sheet to recognize (default: `"METADATA"`).

- ...:

  Additional arguments passed to
  [`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html).

## Value

A named list of tibbles. If `include_metadata = TRUE` and the metadata
sheet was found, it is accessible via `attr(result, "metadata")`.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- read_database_from_excel("clinical_database.xlsx")
meta <- attr(db, "metadata")
} # }
```
