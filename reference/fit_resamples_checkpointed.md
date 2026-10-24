# Checkpointed Model Evaluation Across Resamples (\`fit_resamples_checkpointed\`)

A fingerprinted, resumable wrapper around \[tune::fit_resamples()\] that
evaluates each resample split independently and saves fingerprinted
checkpoints into \`checkpoint_dir\`.

## Usage

``` r
fit_resamples_checkpointed(
  object,
  resamples,
  metrics = NULL,
  control = tune::control_resamples(),
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

  An \`rset\` resampling object.

- metrics:

  A \`yardstick::metric_set()\`.

- control:

  A \`tune::control_resamples()\` object.

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

  Additional arguments passed to \[tune::fit_resamples()\].

## Value

A standard \`resample_results\` object.

## See also

\[tune_grid_checkpointed()\], \[checkpoint_map()\]
