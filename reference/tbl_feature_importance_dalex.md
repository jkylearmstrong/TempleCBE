# Table of DALEX Permutation Feature Importance

Summarises the dropout loss of each variable over the permutations of a
\[DALEX::feature_importance()\] (or \`model_parts()\`) result: minimum,
first quartile, median, mean, third quartile and maximum, ranked by the
chosen statistic. Replaces the hand-rolled aggregation that was repeated
in the feature-selection child documents.

## Usage

``` r
tbl_feature_importance_dalex(
  x,
  rank_by = c("mean", "median", "min", "q1", "q3", "max"),
  top_n = NULL,
  keep_reference = FALSE,
  id = NULL
)
```

## Arguments

- x:

  A \`feature_importance_explainer\` object or a data frame with the
  columns \`variable\`, \`permutation\` and \`dropout_loss\` (and
  optionally \`label\`).

- rank_by:

  Statistic to rank by, one of \`"mean"\` (default), \`"median"\`,
  \`"min"\`, \`"q1"\`, \`"q3"\`, \`"max"\`. Larger dropout loss means
  more important.

- top_n:

  Keep only the \`top_n\` highest ranked variables (default all).

- keep_reference:

  Keep the \`\_baseline\_\` and \`\_full_model\_\` rows (default
  \`FALSE\`).

- id:

  Optional named list of constant columns put in front, e.g.
  \`list(index = "A")\`, to stack the tables of several models with
  \[dplyr::bind_rows()\].

## Value

A tibble with \`rank\`, \`variable\`, \`label\` (if present),
\`dropout_loss\` (the unpermuted, \`permutation == 0\` value, if
present) and the six summary columns.

## See also

\[calc_grouped_summary()\]

## Examples

``` r
fi <- data.frame(
  variable = rep(c("_full_model_", "a", "b", "_baseline_"), each = 4),
  permutation = rep(0:3, 4),
  dropout_loss = c(rep(0.2, 4), 0.30, 0.31, 0.29, 0.33, 0.22, 0.21, 0.23, 0.22, rep(0.5, 4)),
  label = "model"
)
tbl_feature_importance_dalex(fi)
#> # A tibble: 2 × 10
#>    rank variable label dropout_loss   min    q1 median  mean    q3   max
#>   <int> <chr>    <chr>        <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1     1 a        model         0.3   0.29 0.298  0.305 0.308 0.315  0.33
#> 2     2 b        model         0.22  0.21 0.218  0.22  0.22  0.222  0.23
```
