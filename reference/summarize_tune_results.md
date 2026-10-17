# Tune Over \`alpha\` for Every Split of a Resample

Evaluates candidate \`(split, alpha)\` (or \`(split, formula)\`)
combinations as a single flat task grid across all available
\[future::plan()\] workers and row-binds the successful fits.

## Usage

``` r
summarize_tune_results(
  object,
  ...,
  num_alpha_values = 10,
  num_fixed = 6,
  alphas = NULL,
  formulas = NULL,
  progress = FALSE,
  seed = NULL,
  checkpoint_dir = NULL,
  checkpoint_mismatch = c("recompute", "error"),
  shard = NULL
)
```

## Arguments

- object:

  A resampling object with a \`splits\` column (e.g. one element of the
  \`inner_resamples\` column of \[rsample::nested_cv()\]).

- ...:

  Arguments passed to \[glmnet_IBS()\] (\`recipe\`, \`feature_names\`,
  \`time_data\`, \`id_col\`, \`cox.ties\`, ...).

- num_alpha_values:

  Total number of \`alpha\` values in the grid.

- num_fixed:

  Number of evenly spaced values from 0 to 1 in the grid.

- alphas:

  Optional numeric vector of \`alpha\` values to use instead of the
  generated grid; in formula mode, one per formula.

- formulas:

  Optional character vector of \`+\`-separated feature sets, or a list
  of one-sided formulas, each fit as a separate model (see
  \[glmnet_IBS()\]'s \`formula\`).

- progress:

  Show a progress bar.

- seed:

  Optional integer seed for reproducible random draws across splits.

- checkpoint_dir:

  Optional directory path to save checkpoint files. Finished
  splits/tasks are loaded from disk on rerun when their fingerprint
  matches, skipping recomputation.

- checkpoint_mismatch:

  Action when an existing checkpoint file's fingerprint does not match
  the current inputs/settings: \`"recompute"\` (default; warns and
  recomputes) or \`"error"\` (stops immediately).

- shard:

  Optional integer vector \`c(i, n)\` specifying that this call should
  process shard \`i\` of \`n\` across machines into \`checkpoint_dir\`.
  Use \[collect_checkpoints()\] to merge all shards once finished.

## Value

A tibble of \[glmnet_IBS()\] results with an \`inner_resamples_splits\`
column giving the split's identifier.

## Nested Parallelism & Benchmarking

When tuning penalized survival models across nested resampling
structures, computation scales multiplicatively: \\\text{outer folds}
\times \text{inner splits} \times \text{alphas} \times \text{internal CV
folds}\\. \`summarize_tune_results()\` flattens \`inner splits x
alphas\` into a single task pool (e.g. 7 splits x 10 alphas = 70 tasks)
so all cores on high-core machines are fully utilized. Set
`future::plan(multisession, workers = N)` before calling while keeping
inner model evaluations sequential (`parallel = FALSE` in
[`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)).

## See also

\[glmnet_IBS()\], \[tune_over_alpha()\], \[collect_checkpoints()\],
\[nested_cv_coxnet()\]
