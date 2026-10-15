# Score Candidate Models on Outer Resampling Split(s)

A tidy wrapper around \[glmnet_IBS()\] using \[furrr::future_map()\] (or
sequential \[purrr::map()\]) to evaluate a grid of candidate models
across one or more outer resampling splits.

## Usage

``` r
score_outer(
  object,
  grid,
  recipe,
  ...,
  censoring_weights = c("ipcw", "none"),
  coefficients = c("all", "nonzero"),
  seed = NULL,
  checkpoint_dir = NULL,
  progress = FALSE
)
```

## Arguments

- object:

  An \`rsplit\` (e.g. from \[rsample::initial_split()\]), an \`rset\` /
  \[rsample::nested_cv()\] object containing outer splits, or a list of
  \`rsplit\` objects.

- grid:

  A data frame or tibble defining candidate models to evaluate. Must
  contain an \`alpha\` column. If a \`formula\` or \`formulas\` column
  is present, each row's formula is passed to \[glmnet_IBS()\]. An
  optional \`model\` or \`model_id\` column will be preserved as the
  model identifier; if absent, one will be created.

- recipe:

  A recipe object specifying preprocessing.

- ...:

  Additional arguments passed to \[glmnet_IBS()\] (such as
  \`time_data\`, \`internal_folds\`, \`feature_names\`, \`id_col\`,
  \`cox.ties\`, \`covariates\`, \`rule\`, \`metric\`, \`tune_engine\`).

- censoring_weights:

  Weighting scheme: \`"ipcw"\` (default) or \`"none"\`.

- coefficients:

  Option controlling which coefficients to store in the \`coefficients\`
  list column: \`"all"\` (default, includes predictors shrunk to 0) or
  \`"nonzero"\` (filters to non-zero estimates).

- seed:

  Optional integer seed for reproducible model fitting across future
  plans.

- checkpoint_dir:

  Optional directory path to save per-task checkpoint files. Finished
  tasks are loaded from disk on rerun, skipping recomputation.

- progress:

  Show progress bar (default \`FALSE\`).

## Value

A tibble with columns \`outer_fold\`, \`model\`, \`alpha\`, \`formula\`
(if provided), \`IBS\` (integrated Brier score), \`lambda\`, and
\`coefficients\` (list column of coefficient estimates).

## See also

\[glmnet_IBS()\], \[tidy_outer_coefficients()\],
\[compare_censoring_weights()\], \[select_candidate_models()\]
