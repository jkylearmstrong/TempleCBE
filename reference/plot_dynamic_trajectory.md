# Longitudinal Biomarker Trajectories by Cohort / Outcome

Plots mean trajectory over time with standard error bars, stratified by
outcome or treatment cohort (e.g. Survived vs. Died), supporting
arbitrary color scales.

## Usage

``` r
plot_dynamic_trajectory(
  data,
  value_col,
  time_col,
  group_col,
  facet_col = NULL,
  colors = c(Survived = "#6F6F6F", Died = "#9D2235"),
  base_size = 12,
  palette = NULL,
  group_levels = NULL,
  ribbon_type = c("se", "quantile", "iqr"),
  quantile_probs = c(0.025, 0.975),
  central_tendency = c("mean", "median")
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

  Named or unnamed character vector of colors matching group levels, or
  \`NULL\` to use default CBE palette. Defaults to
  `c(Survived = "#6F6F6F", Died = "#9D2235")`. When \`colors\` is left
  at its default and the data's groups are not Survived/Died, the CBE
  palette is used instead of failing, so any grouping works without
  extra arguments.

- base_size:

  Base font size (default: 12).

- palette:

  Optional colors for the groups, used instead of \`colors\`. A
  \*\*named\*\* vector must name every group (extra names are ignored),
  so a group keeps its color whichever subset of groups is plotted. An
  \*\*unnamed\*\* vector is assigned to the groups in level order and
  must be at least as long as the number of groups. Anything short of
  that is an error, not a recycled color.

- group_levels:

  Optional character vector giving the groups and their order (legend
  and color assignment). Every group in the data must be listed; list
  the levels in the order you want. By default a factor keeps its levels
  (unused ones are dropped) and other columns are sorted. Use this for
  multiclass phenotypes, tertiles (\`c("T1", "T2", "T3")\`) or cluster
  labels.

- ribbon_type:

  How the spread around the central line is drawn: \`"se"\` (default;
  error bars of the standard error of the mean, as before),
  \`"quantile"\` (a ribbon between the empirical \`quantile_probs\` of
  the values at each time point) or \`"iqr"\` (a ribbon from the 25th to
  the 75th percentile). Quantiles use \[stats::quantile()\] type 7. A
  time point with one value has no spread.

- quantile_probs:

  Two probabilities (lower, upper) for \`ribbon_type = "quantile"\`;
  default \`c(0.025, 0.975)\`.

- central_tendency:

  \`"mean"\` (default) or \`"median"\`, the line and points. With
  \`ribbon_type = "se"\` the error bars are centred on the line, so with
  \`"median"\` they are median plus or minus the standard error of the
  mean; prefer a quantile ribbon with the median.

## Value

A ggplot2 object.
