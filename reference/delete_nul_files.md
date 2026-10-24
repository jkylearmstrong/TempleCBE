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

  Logical (default `FALSE`); if `TRUE`, return the paths that would be
  deleted, in device-namespace form, and delete nothing.

## Value

Invisibly, the deleted file paths (or, if `.verify_command = TRUE`, the
paths that would be deleted). An error lists any file that could not be
deleted.

## Details

The files are deleted from R, through their Windows device-namespace
path (a plain path to such a file is taken for the `NUL` device itself),
not by handing a command to `cmd.exe`, so a folder name that holds
percent signs, such as `x%TEMP%y`, is safe. Folders that are reached
through a link (a junction or symbolic link inside `path` that points
elsewhere) are skipped, so nothing outside `path` is deleted.
