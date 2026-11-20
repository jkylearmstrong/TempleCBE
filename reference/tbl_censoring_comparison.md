# Render Censoring Weight Sensitivity and Rank Agreement Table

Formats the output of \[compare_censoring_weights()\] (or evaluates it
directly on candidate models) into a tidy sensitivity table suitable for
inclusion in Quarto and R Markdown reports. Reports integrated Brier
scores under unweighted and IPCW weighting alongside model ranks, rank
differences, and rank agreement metrics (Spearman's \\\rho\\ and
Kendall's \\\tau\\).

## Usage

``` r
tbl_censoring_comparison(
  data,
  grid = NULL,
  recipe = NULL,
  ...,
  format = c("tibble", "gt"),
  digits = 4,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- data:

  A data frame returned by \[compare_censoring_weights()\], or an object
  passed to \[compare_censoring_weights()\] (such as an \`rsplit\`,
  \`nested_cv\`, or list).

- grid:

  Optional candidate model grid if \`data\` is an \`rsplit\`/resamples
  object.

- recipe:

  Optional preprocessing recipe if \`data\` is an \`rsplit\`/resamples
  object.

- ...:

  Additional arguments passed to \[compare_censoring_weights()\].

- format:

  Output format: \`"tibble"\` (default) or \`"gt"\` (requires gt).

- digits:

  Number of decimal places for numeric metrics (default 4).

- label_map:

  Optional label mapping passed to \[resolve_cbe_labels()\].

- style:

  Optional label style (\`"short"\` or \`"full"\`) when resolving
  \`label_map\`.

## Value

A tibble or `gt_tbl` object displaying side-by-side IBS, ranks, rank
differences, and rank agreement correlation statistics.

## See also

\[compare_censoring_weights()\], \[score_outer()\]
