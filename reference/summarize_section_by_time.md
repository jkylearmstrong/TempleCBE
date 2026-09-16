# Summarize One Section By Its Time Variable

Generates summary tables (using
[`arsenal::tableby`](https://mayoverse.github.io/arsenal/reference/tableby.html))
grouped by whichever column is flagged with `Time_var == TRUE` in the
mapping dictionary.

## Usage

``` r
summarize_section_by_time(df, mapping, index, id_cols = NULL)
```

## Arguments

- df:

  Data frame already renamed via
  [`read_mapped_section_data`](https://jkylearmstrong.github.io/TempleCBE/reference/read_mapped_section_data.md).

- mapping:

  Validated column mapping table.

- index:

  Character string identifying the section/domain.

- id_cols:

  Column names to exclude from summary (defaults to columns with
  `ID_var == TRUE`).

## Value

An
[`arsenal::tableby`](https://mayoverse.github.io/arsenal/reference/tableby.html)
summary object.
