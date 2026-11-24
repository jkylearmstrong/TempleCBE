# Summarize LOCO-MP Importance Across Outer Folds

Summarizes, per term, the leave-one-covariate-out minipatch importance
that \[nested_cv_coxnet()\] computes on each outer split with
\`importance = "loco_mp"\`.

## Usage

``` r
summarize_loco_mp(x, alpha = 0.05, label_map = NULL, style = NULL)
```

## Arguments

- x:

  A \[nested_cv_coxnet()\] result with an \`.importance\` column, the
  long tibble from \`tidy_nested_cv_coxnet(x, what = "importance")\`, or
  a \[loco_mp_coxnet()\] fit.

- alpha:

  Significance level for \`n_significant\`, applied to \`p_adjusted\`
  (the one-sided, multiplicity-adjusted p-value).

- label_map, style:

  Passed to \[resolve_cbe_labels()\].

## Value

A tibble with one row per term: \`term\`, \`label\`, \`n_folds\` (outer
splits with an estimate), \`mean_importance\`, \`median_importance\`,
\`sd_importance\`, \`min_importance\`, \`max_importance\`,
\`mean_std_error\`, \`n_significant\` (folds with \`p_adjusted \<
alpha\`) and \`prop_significant\`, ordered by descending
\`median_importance\`.

## See also

\[tidy_nested_cv_coxnet()\], \[loco_mp_coxnet()\]
