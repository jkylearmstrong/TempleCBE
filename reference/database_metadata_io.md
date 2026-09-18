# Export or Import Dataset Metadata

Exports and imports data dictionaries/metadata to and from `.csv` or
`.xlsx` formats for in-place review and editing.

## Usage

``` r
write_database_metadata(metadata, path, ...)

read_database_metadata(path, sheet = NULL, ...)
```

## Arguments

- metadata:

  A tibble/data.frame of dataset metadata (e.g. from
  [`get_dataset_info`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)).

- path:

  File path ending in `.csv` or `.xlsx`.

- ...:

  Additional arguments passed to the underlying writer/reader.

- sheet:

  Optional sheet name when reading from Excel (defaults to 1 or
  `"METADATA"`).

## Value

For writers, `path` invisibly. For readers, a tibble of metadata.
