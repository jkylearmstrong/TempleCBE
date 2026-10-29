# Assemble the Final Imputed Data From the Winning Methods

Builds one data frame in which every variable comes from the imputation
method that won for it (\[tbl_best_imputation_method()\]).

## Usage

``` r
build_best_imputed_data(
  data,
  imputations,
  best,
  require_beats_null = FALSE,
  fallback = NULL
)
```

## Arguments

- data:

  The original data frame (with its missing values). Columns that are
  not in \`best\` are returned as they are.

- imputations:

  A \*\*named list\*\* of data frames, one per method, each the same
  size as \`data\` and in the same row order (this is not checked beyond
  the number of rows) with the columns imputed by that method. The names
  are the method names used in \`best\`.

- best:

  A \[tbl_best_imputation_method()\] result.

- require_beats_null:

  If \`TRUE\`, a variable whose best method does not beat the null
  baseline (RSE of 1 or more) is left with the \`fallback\` method
  instead of the winner.

- fallback:

  Method name (in \`imputations\`) used for variables without a winner
  and, with \`require_beats_null\`, for those that do not beat the null
  baseline. \`NULL\` (default) leaves such variables as in \`data\`.

## Value

A data frame like \`data\`, with attribute \`"imputation_source"\`: a
tibble of \`variable\` and \`method\` (\`NA\` where the column came from
\`data\`).
