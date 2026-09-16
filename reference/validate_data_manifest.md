# Validate Data Snapshot Copies Against Upstream Sources

Compares destination files against upstream source files checking: 1.
Existence of both source and copy. 2. Cryptographic MD5 hash equality.
3. Object-level equivalence via `identical(readRDS(), readRDS())` for
`.rds` files.

## Usage

``` r
validate_data_manifest(dir = ".", manifest = NULL, project_root = here::here())
```

## Arguments

- dir:

  Destination directory containing the snapshot copies.

- manifest:

  Data frame with columns `file` and `source`.

- project_root:

  Root directory for resolving `source` paths (default:
  [`here::here()`](https://here.r-lib.org/reference/here.html)).

## Value

A tibble with validation status per file.
