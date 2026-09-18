# Write an R Database or Data Frames to an Excel Workbook

Writes a named list of data frames (an R database) or a single data
frame to a multi-worksheet Excel workbook, with an optional automated
data dictionary worksheet.

## Usage

``` r
write_database_to_excel(
  x,
  path,
  include_metadata = TRUE,
  metadata = NULL,
  metadata_sheet = "METADATA",
  ...
)

write_workbook(
  x,
  path,
  include_metadata = TRUE,
  metadata = NULL,
  metadata_sheet = "METADATA",
  ...
)

cbe_write_database(
  x,
  path,
  include_metadata = TRUE,
  metadata = NULL,
  metadata_sheet = "METADATA",
  ...
)
```

## Arguments

- x:

  A named list of data frames (an R database) or a single data
  frame/tibble.

- path:

  File path to save the `.xlsx` workbook.

- include_metadata:

  Logical; if `TRUE` (default), computes and appends a data dictionary
  sheet generated via
  [`get_dataset_info`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md).

- metadata:

  Optional pre-computed metadata data frame. If `NULL` and
  `include_metadata = TRUE`, metadata is computed automatically.

- metadata_sheet:

  Character string naming the metadata worksheet (default:
  `"METADATA"`).

- ...:

  Additional arguments passed to
  [`write_xlsx`](https://docs.ropensci.org/writexl//reference/write_xlsx.html).

## Value

The file path invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- list(patients = data.frame(id = 1:3), labs = data.frame(id = 1:3, val = 4:6))
write_database_to_excel(db, "clinical_database.xlsx")
} # }
```
