# Summary Table of Permutation Variable Importance

Returns a formatted table of feature importance rankings and dropout
losses.

## Usage

``` r
tbl_permutation_importance(data, top_n = NULL)
```

## Arguments

- data:

  A data frame from \[summarize_dalex_importance()\].

- top_n:

  Optional integer limit on the number of features to return.

## Value

A tibble with \`rank\`, \`variable\`, \`importance\`, and
\`dropout_loss\`.
