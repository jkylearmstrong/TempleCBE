# Audit Data File Read/Write Calls Against a Project's Files on Disk

Scans `code_path` for read and write calls (via
[`read_search`](https://jkylearmstrong.github.io/TempleCBE/reference/read_search.md)/[`write_search`](https://jkylearmstrong.github.io/TempleCBE/reference/write_search.md)),
resolves the file paths those calls reference — including
[`here::here()`](https://here.r-lib.org/reference/here.html) calls, and
a heuristic fallback for indirect references like
`excel_file_paths[["ABG"]]` matched against filenames actually present
on disk — and cross-references them against every matching file under
`project_root`. The result classifies each such file as a write output
the code produces, a read input the code consumes, both, or an orphan
with no code reference found (`"unknown"`) — useful for finding stale,
missing, or undocumented data deliverables in a project.

## Usage

``` r
scan_data_io(
  code_path,
  project_root = code_path,
  ext = "xlsx",
  strong_read_patterns = c("read_workbook", "read_excel_multiple_headers"),
  include_comments_write = FALSE,
  max_depth = Inf
)
```

## Arguments

- code_path:

  Directory of scripts (`.R`/`.Rmd`/`.qmd`) to search for read/write
  calls.

- project_root:

  Root directory to inventory files under, and to resolve
  relative/[`here::here()`](https://here.r-lib.org/reference/here.html)
  paths against (useful when auditing a checkout at a different location
  than the one the scripts were written against). Defaults to
  `code_path`.

- ext:

  File extension to audit, without the leading dot (e.g. `"xlsx"`,
  `"csv"`, `"rds"`). Defaults to `"xlsx"`.

- strong_read_patterns:

  Character vector of
  [`read_search`](https://jkylearmstrong.github.io/TempleCBE/reference/read_search.md)
  pattern names that should always count as a read of a file with this
  extension, even on a line that doesn't otherwise mention `ext` (e.g. a
  call to a project-specific reader function). Defaults to
  `c("read_workbook", "read_excel_multiple_headers")`.

- include_comments_write:

  Logical, passed through to
  [`write_search`](https://jkylearmstrong.github.io/TempleCBE/reference/write_search.md).
  Defaults to `FALSE`.

- max_depth:

  Maximum folder depth below `project_root` to inventory files in
  (default `Inf`, no limit).

## Value

A list with:

- writes:

  Resolved file-write call sites.

- inputs:

  Resolved file-read call sites.

- missing_write_dirs:

  Directories containing a write target that doesn't exist on disk yet,
  and any matching-extension files present there now — a likely spot for
  a not-yet-(re)generated deliverable.

- files:

  Every file with extension `ext` under `project_root`, tagged
  `write_output`/`workflow_input`/ `unknown` and by which script(s)
  reference it.

## Examples

``` r
if (FALSE) { # \dontrun{
scan_data_io(here::here("analysis"), ext = "xlsx")
} # }
```
