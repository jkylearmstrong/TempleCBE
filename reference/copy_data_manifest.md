# Copy Datasets to Snapshot Directory

Copies files listed in a manifest from their source paths to a
destination directory, preserving timestamps and validating contents
immediately upon copy.

## Usage

``` r
copy_data_manifest(
  dir = ".",
  manifest = NULL,
  project_root = here::here(),
  overwrite = TRUE
)
```

## Arguments

- dir:

  Destination directory for frozen copies.

- manifest:

  Data frame with columns `file` and `source`. If NULL, read from
  `file.path(dir, "manifest.csv")`.

- project_root:

  Absolute or relative root directory for resolving `source` paths
  (default:
  [`here::here()`](https://here.r-lib.org/reference/here.html)).

- overwrite:

  Logical; whether to overwrite existing destination files (default:
  TRUE).

## Value

A validation tibble produced by
[`validate_data_manifest`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_data_manifest.md).
