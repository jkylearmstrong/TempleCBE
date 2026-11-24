# Search a Directory Tree for File-Write Calls

Convenience wrapper around
[`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md)
that searches for common data-export function calls (`write_xlsx`,
`saveRDS`, `write.csv`, etc.) across a directory tree.

## Usage

``` r
find_write_calls(path, include_comments = FALSE, workers = NULL)
```

## Arguments

- path:

  Character. Root directory to search.

- include_comments:

  Logical. Passed through to
  [`find_code`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md).
  Defaults to `FALSE`.

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
with an additional `pattern` column recording which write function
matched.

## Examples

``` r
if (FALSE) { # \dontrun{
find_write_calls(here::here("analysis"))
} # }
```
