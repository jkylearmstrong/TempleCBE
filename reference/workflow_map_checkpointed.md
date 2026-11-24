# Checkpointed Workflow Sets Mapping (\`workflow_map_checkpointed\`)

A resumable, fingerprinted wrapper around
\[workflowsets::workflow_map()\] that checkpoints each workflow's tuning
results independently into subdirectories of \`checkpoint_dir\`.

## Usage

``` r
workflow_map_checkpointed(
  object,
  fn = "tune_grid",
  ...,
  seed = 1L,
  verbose = FALSE,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  progress = FALSE
)
```

## Arguments

- object:

  A \`workflow_set\` object.

- fn:

  Function to apply to each workflow (default \`"tune_grid"\`). The
  start/stop drivers \`"tune_grid_start_stop"\`,
  \`"tune_race_start_stop"\` and \`"fit_resamples_start_stop"\` are
  mapped here too (with or without \`checkpoint_dir\`), since
  \[workflowsets::workflow_map()\] accepts only tune's functions; pass
  \`resamples\` and \`subject_id\` in \`...\`.

- ...:

  Arguments passed to \`fn\`, for every workflow (they override the
  workflow's own \[workflowsets::option_add()\] options of the same
  name).

- seed:

  Optional integer seed (default \`1L\`) used to set a reproducible RNG
  seed per workflow.

- verbose:

  Logical (default \`FALSE\`). Display progress from
  \[workflowsets::workflow_map()\].

- checkpoint_dir:

  Directory to store per-workflow checkpoints. They are read back with
  \[readRDS()\]: use a directory that only trusted processes write to.
  Each workflow's result is stored with a fingerprint of that workflow,
  the function and its arguments, so a changed workflow or setting does
  not silently reuse a stale cached result (including for workflows this
  shard did not evaluate).

- shard:

  Optional integer vector \`c(i, n)\` or string \`"i/n"\` with \`1 \<= i
  \<= n\`: evaluate shard \`i\` of \`n\`. Anything else is an error.

- checkpoint_mismatch:

  Action on fingerprint mismatch.

- progress:

  Logical (default \`FALSE\`).

## Value

A \`workflow_set\` object with populated result columns.

## Details

Each workflow is run with its own options, as set with
\[workflowsets::option_add()\] (\`param_info\`, \`grid\`, ...), combined
with the arguments given in \`...\` the way
\[workflowsets::workflow_map()\] combines them: an argument given in
\`...\` replaces an option of the same name. The options are part of
each workflow's fingerprint, so changing one recomputes that workflow
only. For \`fn = "fit_resamples"\` the tuning-only options
(\`param_info\`, \`grid\`, ...) are dropped, as in \`workflow_map()\`.

## See also

\[tune_grid_checkpointed()\], \[workflowsets::workflow_map()\]
