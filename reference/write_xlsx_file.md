# Write Data Frame to Excel (.xlsx) File

A non-masking wrapper around \[writexl::write_xlsx()\] to write data
frames or lists of data frames to Excel files.

## Usage

``` r
write_xlsx_file(x, path = tempfile(fileext = ".xlsx"), ...)
```

## Arguments

- x:

  A data frame or named list of data frames that will be written to
  Excel sheets.

- path:

  Path to the output Excel \`.xlsx\` file.

- ...:

  Additional arguments passed to \[writexl::write_xlsx()\].

## Value

The normalized path to the written file, invisibly.

## See also

\[writexl::write_xlsx()\], \[write_database_to_excel()\]

## Examples

``` r
# \donttest{
tmp <- tempfile(fileext = ".xlsx")
write_xlsx_file(mtcars, tmp)
unlink(tmp)
# }
```
