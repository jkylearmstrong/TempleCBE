# Group Comparison Bar Chart with Standard Errors

Faceted bar chart comparing biomarker means and standard errors across
groups.

## Usage

``` r
plot_group_comparison(
  data,
  value_col,
  group_col,
  facet_col = NULL,
  se_col = NULL,
  colors = c(Survived = "#6F6F6F", Died = "#9D2235"),
  base_size = 13
)
```

## Arguments

- data:

  Data frame containing summary means and standard errors (or raw data).

- value_col:

  Column containing index values or means.

- group_col:

  Cohort/outcome grouping column (e.g. "outcome_label").

- facet_col:

  Column to facet wrap across (e.g. "index_label").

- se_col:

  Optional standard error column. If NULL, calculated automatically.

- colors:

  Named color vector (default:
  `c(Survived = "#6F6F6F", Died = "#9D2235")`).

- base_size:

  Base font size (default: 13).

## Value

A ggplot2 object.
