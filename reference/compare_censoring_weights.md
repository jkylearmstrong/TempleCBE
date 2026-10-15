# Compare Model Performance Under Unweighted and IPCW Censoring Weights

Evaluates candidate models on an outer resampling split under both
\`"none"\` (unweighted legacy scoring) and \`"ipcw"\` (proper inverse
probability of censoring weighted scoring), returning a tidy
side-by-side comparison table of integrated Brier scores (IBS) and model
ranks, along with rank agreement statistics (Spearman rank correlation
and Kendall's tau).

## Usage

``` r
compare_censoring_weights(
  object,
  grid,
  recipe = NULL,
  ...,
  seed = NULL,
  progress = FALSE
)
```

## Arguments

- object:

  An \`rsplit\`, \`nested_cv\`, list of \`rsplit\`s, or a precomputed
  data frame scored under \`censoring_weights = "none"\`.

- grid:

  A candidate model grid (data frame with \`alpha\` and optional
  \`formula\`), or a precomputed data frame scored under
  \`censoring_weights = "ipcw"\` when \`object\` is already a scored
  data frame.

- recipe:

  Preprocessing recipe passed to \[score_outer()\].

- ...:

  Additional arguments passed to \[glmnet_IBS()\] through
  \[score_outer()\].

- seed:

  Optional integer seed for reproducible draws and fitting.

- progress:

  Show progress bar (default \`FALSE\`).

## Value

A tibble with columns:

- outer_fold:

  Outer fold identifier (if present)

- model:

  Model identifier

- alpha:

  Elastic net mixing parameter

- formula:

  Formula specification (if present)

- ibs_none:

  Integrated Brier score under \`censoring_weights = "none"\`

- ibs_ipcw:

  Integrated Brier score under \`censoring_weights = "ipcw"\`

- rank_none:

  Model rank under unweighted IBS (1 = best / lowest IBS)

- rank_ipcw:

  Model rank under IPCW IBS (1 = best / lowest IBS)

- rank_diff:

  \`rank_ipcw - rank_none\`

The returned object has attribute \`"rank_agreement"\` containing a
named list with \`spearman\` (Spearman's rank correlation) and
\`kendall\` (Kendall's rank correlation).

## Details

Alternatively, accepts two precomputed tables of model evaluations (one
under \`"none"\` and one under \`"ipcw"\`) and merges them into the
comparison table.

## See also

\[score_outer()\], \[glmnet_IBS()\], \[select_candidate_models()\]
