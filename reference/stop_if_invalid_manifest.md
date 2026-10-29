# Stop Execution if Data Copies are Invalid or Stale

Halts report execution with an actionable error message if any snapshot
copy does not match its upstream source.

## Usage

``` r
stop_if_invalid_manifest(validation, dir = ".")
```

## Arguments

- validation:

  Validation tibble returned by
  [`validate_data_manifest`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_data_manifest.md).

- dir:

  Destination directory name for reporting.

## Value

Invisibly returns `validation` if all files are valid.
