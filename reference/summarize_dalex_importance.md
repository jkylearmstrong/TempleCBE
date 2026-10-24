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

  A DALEX explainer object, or a fitted model / function supported by
  DALEX.

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
and \`rank\`.

## See also

\[plot_permutation_importance()\], \[tbl_permutation_importance()\]
