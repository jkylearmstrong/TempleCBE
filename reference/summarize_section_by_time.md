# Summarize One Section By Its Time Variable

Generates summary tables (using gtsummary by default, or optionally
arsenal) grouped by whichever column is flagged with `Time_var == TRUE`
in the mapping dictionary.

## Usage

``` r
summarize_section_by_time(
  df,
  mapping,
  index,
  id_cols = NULL,
  engine = c("gtsummary", "arsenal")
)
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

- engine:

  Character string specifying the summary engine: `"gtsummary"` (the
  default) or `"arsenal"`.

## Value

A
[`tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
object (when `engine = "gtsummary"`) or an
[`arsenal::tableby`](https://mayoverse.github.io/arsenal/reference/tableby.html)
summary object (when `engine = "arsenal"`).
