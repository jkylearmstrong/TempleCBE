# Search for Code Patterns Across a Directory Tree

Recursively searches R-related files (`.R`, `.Rmd`, `.qmd`, etc.) under
`directory` for a string or regex pattern.

## Usage

``` r
find_code(
  directory,
  pattern,
  lines_before = 0,
  lines_after = 0,
  regex = FALSE,
  ignore_case = FALSE,
  include_comments = TRUE,
  exts = c("R", "r", "Rmd", "rmd", "qmd"),
  exclude_dirs = c(".git", "renv", "_freeze", "_book", "_site", ".quarto", "_extensions",
    "cache", ".Rproj.user"),
  return_all_matches = FALSE,
  match_extractor = NULL
)
```

## Arguments

- directory:

  Character. Root directory to search.

- pattern:

  Character. String or regex to search for.

- lines_before, lines_after:

  Integers. Context lines around each hit.

- regex:

  Logical. If `TRUE`, treat `pattern` as a Perl regex.

- ignore_case:

  Logical. Case-insensitive when `TRUE` (works correctly even in
  fixed-string mode, where base
  [`grep`](https://rdrr.io/r/base/grep.html)'s own `ignore.case`
  argument is silently ignored).

- include_comments:

  Logical. If `FALSE`, skip comment-only lines.

- exts:

  Character vector of file extensions to include.

- exclude_dirs:

  Character vector of directory names to exclude.

- return_all_matches:

  Logical. If `TRUE`, returns one row per extracted match token (via
  `match_extractor`) instead of one row per matching line.

- match_extractor:

  Optional `function(content_subset)` returning a list of tokens per
  line; only used when `return_all_matches = TRUE`.

## Value

A tibble: `file`, `path`, `line_number`, `line`, and (when
`return_all_matches = TRUE`) `match`.

## Examples

``` r
find_code(system.file("R", package = "TempleCBE"), "roxygen2", ignore_case = TRUE)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: file <chr>, path <chr>, line_number <int>, line <chr>
```
