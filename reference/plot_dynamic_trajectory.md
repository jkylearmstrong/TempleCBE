# Longitudinal Biomarker Trajectories by Cohort / Outcome

Plots mean trajectory over time with standard error bars, stratified by
outcome or treatment cohort (e.g. Survived vs. Died).

## Usage

``` r
plot_dynamic_trajectory(
  data,
  value_col,
  time_col,
  group_col,
  facet_col = NULL,
  colors = c(Survived = "#6F6F6F", Died = "#9D2235"),
  base_size = 12
)
```

## Arguments

- data:

  Long-format data frame containing longitudinal measurements.

- value_col:

  Character name of measurement column.

- time_col:

  Character name of time point column.

- group_col:

  Character name of cohort/grouping column.

- facet_col:

  Optional character name of domain or biomarker column to facet wrap.

- colors:

  Named character vector of colors matching group levels. Defaults to
  `c(Survived = "#6F6F6F", Died = "#9D2235")`.

- base_size:

  Base font size (default: 12).

## Value

A ggplot2 object.
