# Extract and Summarize Permutation Variable Importance

Computes permutation feature importance from a fitted model or DALEX
explainer, returning a tidy table of variable importance rankings and
dropout losses.

## Usage

``` r
summarize_dalex_importance(explainer, data = NULL, y = NULL, B = 10, ...)
```

## Arguments

- explainer:

  A DALEX explainer object, a ready-made permutation importance result
  (\[DALEX::model_parts()\] / \`feature_importance()\`, or
  \`survex::model_parts()\` for a survival model, or a data frame with
  \`variable\`, \`permutation\` and \`dropout_loss\`), or a fitted model
  with coefficients. For a result object the summary is taken over its
  permutations; \`data\`, \`y\`, \`B\` and \`...\` are not used.

- data:

  Optional test dataset for computing importance.

- y:

  Optional outcome vector if creating an explainer on the fly.

- B:

  Number of permutation rounds (default 10).

- ...:

  Additional arguments passed to \[DALEX::model_parts()\].

## Value

A tibble with columns \`variable\`, \`dropout_loss\`, \`importance\`,
and \`rank\`. For a permutation importance result it also has the
distribution of the dropout loss over the permutations (\`min\`, \`q1\`,
\`median\`, \`mean\`, \`q3\`, \`max\`), and the per-permutation values
are kept in \`attr(, "permutations")\` for
\[plot_permutation_importance()\].

## Survival models

\`survex::model_parts()\` returns the loss at every evaluation time, in
one column per variable. The summary averages each permutation over the
evaluation times, so one number per variable and permutation remains.

## See also

\[plot_permutation_importance()\], \[tbl_permutation_importance()\],
\[tbl_feature_importance_dalex()\], \[adapter_tidymodels_survex()\]
