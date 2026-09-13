# Search a Directory Tree for File-Write Calls

Convenience wrapper around
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md)
that searches for common data-export function calls (`write_xlsx`,
`saveRDS`, `write.csv`, etc.) across a directory tree.

## Usage

``` r
write_search(path, include_comments = FALSE, workers = NULL)
```

## Arguments

- path:

  Character. Root directory to search.

- include_comments:

  Logical. Passed through to
  [`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md).
  Defaults to `FALSE`.

- workers:

  Integer number of parallel workers to use when future/furrr are
  installed. Defaults to `NULL`, which caps at available cores minus
  one. Falls back to sequential search when future/furrr are not
  installed.

## Value

A tibble of matches (see
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md))
with an additional `pattern` column recording which write function
matched.

## Examples

``` r
if (FALSE) { # \dontrun{
write_search(here::here("analysis"))
} # }
```
