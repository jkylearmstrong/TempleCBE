# Summary Table of Tuning Performance Across Alpha Values

Calculates summary statistics (mean, SD, median, IQR, min, max) of the
Integrated Brier Score for each evaluated \`alpha\` value.

## Usage

``` r
tbl_tune_alpha(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[tune_alpha_by_split()\].

- label_map:

  Optional label mapping, in a form \[cbe_labels()\] takes. The column
  names are looked up as terms in the map (\`"alpha"\`, \`"n_fits"\`,
  \`"mean_ibs"\`, \`"sd_ibs"\`, \`"median_ibs"\`, \`"iqr_ibs"\`,
  \`"min_ibs"\`, \`"max_ibs"\`); a column whose name is in the map takes
  its label, and any other column keeps its default name. Only a map
  given here renames columns: the map set for the session with
  \[cbe_labels()\] is not used, so that the column names of the result
  do not depend on session state.

- style:

  Optional label style (\`"short"\` or \`"full"\`): which label from
  \`label_map\` a renamed column takes. It has no effect without
  \`label_map\`.

## Value

A tibble of per-alpha IBS statistics. Columns renamed through
\`label_map\` carry their mapped labels as names.
