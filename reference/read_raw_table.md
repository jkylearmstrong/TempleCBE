# Read Raw Table with Multi-Row Header Support

Read Raw Table with Multi-Row Header Support

## Usage

``` r
read_raw_table(file, sheet = NULL, header_rows = 1)
```

## Arguments

- file:

  Path to .csv or .xlsx file.

- sheet:

  Sheet name or number (for Excel).

- header_rows:

  Number of rows to combine into column headers (default: 1).

## Value

A tibble with raw data.
