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

  Function to apply to each workflow (default \`"tune_grid"\`).

- ...:

  Arguments passed to \`fn\`.

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

## See also

\[tune_grid_checkpointed()\], \[workflowsets::workflow_map()\]
