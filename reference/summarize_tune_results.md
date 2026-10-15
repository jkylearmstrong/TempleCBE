# Tune Over \`alpha\` for Every Split of a Resample

Calls \[tune_over_alpha()\] on each split of \`object\` and row-binds
the successful fits. Fits that errored are dropped. A split draws its
own random \`alpha\` values, reproducibly: the map runs with
\`furrr_options(seed = TRUE)\`.

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
  checkpoint_dir = NULL
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

  Optional directory path to save per-split checkpoint files. Finished
  splits are loaded from disk on rerun, skipping recomputation.

## Value

A tibble of \[glmnet_IBS()\] results with an \`inner_resamples_splits\`
column giving the split's position in \`object\$splits\`.

## Nested Parallelism & Benchmarking

When tuning penalized survival models across nested resampling
structures, computation scales multiplicatively: \\\text{outer folds}
\times \text{inner splits} \times \text{alphas} \times \text{internal CV
folds}\\. Parallelism is managed via future and furrr. For optimal
cluster throughput and to prevent socket exhaustion on Windows/macOS,
set `future::plan(multisession, workers = N)` at the outer level (e.g.
over splits) while keeping nested inner model evaluations sequential
(`parallel = FALSE` in
[`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)).
Using `checkpoint_dir` is strongly recommended for long runs across
multiple nodes.

## See also

\[glmnet_IBS()\], \[tune_over_alpha()\], \[nested_cv_coxnet()\]
