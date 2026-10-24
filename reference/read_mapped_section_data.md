# Read One Section's Data Using a Column Mapping

Generic schema-enforced reader: reads whichever file corresponds to an
`INDEX` value in a mapping table, renames raw headers to standardized
names, resolves duplicate columns per `duplicate_action`, and tolerates
missing or unmapped columns.

## Usage

``` r
read_mapped_section_data(
  mapping,
  index,
  file = NULL,
  data_dir = NULL,
  sheet = NULL,
  header_rows = 1
)
```

## Arguments

- mapping:

  Validated column mapping table.

- index:

  Character string identifying the section/domain in `mapping$INDEX`.

- file:

  Optional explicit path to the data file.

- data_dir:

  Directory to search for the section's file if `file` is NULL.

- sheet:

  Sheet name or number for Excel files.

- header_rows:

  Number of header rows to concatenate (default: 1).

## Value

A tibble with standardized column names.
