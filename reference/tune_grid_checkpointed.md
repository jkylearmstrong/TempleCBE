# Checkpointed Tuning Over Resamples (\`tune_grid_checkpointed\`)

A fingerprinted, resumable wrapper around \[tune::tune_grid()\] that
evaluates tuning on a per-resample unit of work. Completed resample
results are saved into \`checkpoint_dir\` and skipped on rerun. Returns
an ordinary \`tune_results\` object so downstream functions like
\[tune::select_best()\], \[tune::collect_metrics()\],
\[tune::finalize_workflow()\], and \[ggplot2::autoplot()\] work
identically.

## Usage

``` r
tune_grid_checkpointed(
  object,
  resamples,
  grid = 10,
  metrics = NULL,
  control = tune::control_grid(),
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  progress = FALSE,
  ...
)
```

## Arguments

- object:

  A \`workflow\` or model specification.

- resamples:

  An \`rset\` resampling object (e.g. from \[rsample::vfold_cv()\]).

- grid:

  Number of grid points or a parameter grid data frame.

- metrics:

  A \`yardstick::metric_set()\`.

- control:

  A \`tune::control_grid()\` object.

- checkpoint_dir:

  Directory to store per-resample fingerprinted checkpoints.

- shard:

  Optional integer vector \`c(i, n)\` for distributed evaluation.

- checkpoint_mismatch:

  Action when a checkpoint fingerprint mismatches: \`"recompute"\`
  (default), \`"overwrite"\`, or \`"error"\`.

- progress:

  Logical (default \`FALSE\`).

- ...:

  Additional arguments passed to \[tune::tune_grid()\].

## Value

A standard \`tune_results\` object.

## See also

\[fit_resamples_checkpointed()\], \[workflow_map_checkpointed()\],
\[checkpoint_map()\]
