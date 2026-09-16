# Read Data Manifest File

Reads a CSV manifest file listing frozen dataset copies and their
repo-relative sources. Expected columns: `file` (filename in destination
folder) and `source` (repo-relative path).

## Usage

``` r
read_data_manifest(dir = ".", manifest_file = "manifest.csv")
```

## Arguments

- dir:

  Directory containing the manifest file (default: current directory).

- manifest_file:

  Name of the manifest CSV file (default: `"manifest.csv"`).

## Value

A data frame containing the manifest entries.
