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
find_read_calls(path, include_comments = TRUE, workers = NULL)
```

## Arguments

- path:

  Character. Root directory to search.

- include_comments:

  Logical. Passed through to
  [`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md).
  Defaults to `TRUE`.

- workers:

  Integer number of parallel workers. With a number greater than 1 (and
  future/furrr installed) the search runs on that many `multisession`
  workers, and the previous
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is put back afterwards. The default `NULL` does not touch the plan:
  the search uses the plan you have set, which is sequential unless you
  chose otherwise.

## Value

A tibble of matches (see
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md))
with an additional `pattern` column recording which read function
matched.

## Examples

``` r
if (FALSE) { # \dontrun{
find_read_calls(here::here("analysis"))
} # }
```
