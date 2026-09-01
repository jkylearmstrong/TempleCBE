# Delete Stray 'nul' Files

Windows-only. `knitr` occasionally leaves behind a file literally named
`nul` as a side effect of redirecting output to the Windows `NUL`
device. This deletes files whose \*basename\* is exactly `"nul"`
(case-insensitive) under `path`.

## Usage

``` r
delete_nul_files(
  path = here::here(),
  .dontask = FALSE,
  .verify_command = FALSE
)
```

## Arguments

- path:

  Directory to search, defaults to
  [`here::here()`](https://here.r-lib.org/reference/here.html).

- .dontask:

  Logical (default `FALSE`); skip the confirmation prompt.

- .verify_command:

  Logical (default `FALSE`); if `TRUE`, return the shell command(s)
  instead of running them.

## Value

Invisibly, the deleted file paths (or the commands, if
`.verify_command = TRUE`).
