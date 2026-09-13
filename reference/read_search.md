# Search a Directory Tree for File-Read Calls

Convenience wrapper around
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md)
that searches for common data-import function calls (`readRDS`,
`read.csv`, `read_excel`,
[`read_workbook`](https://jkylearmstrong.github.io/TempleCBE/reference/read_workbook.md),
[`read_excel_multiple_headers`](https://jkylearmstrong.github.io/TempleCBE/reference/read_excel_multiple_headers.md),
etc.) across a directory tree.

## Usage

``` r
read_search(path, include_comments = TRUE, workers = NULL)
```

## Arguments

- path:

  Character. Root directory to search.

- include_comments:

  Logical. Passed through to
  [`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md).
  Defaults to `TRUE`.

- workers:

  Integer number of parallel workers to use when future/furrr are
  installed. Defaults to `NULL`, which caps at available cores minus
  one. Falls back to sequential search when future/furrr are not
  installed.

## Value

A tibble of matches (see
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md))
with an additional `pattern` column recording which read function
matched.

## Examples

``` r
if (FALSE) { # \dontrun{
read_search(here::here("analysis"))
} # }
```
