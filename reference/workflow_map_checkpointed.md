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

  Directory to store per-workflow checkpoints.

- shard:

  Optional integer vector \`c(i, n)\`.

- checkpoint_mismatch:

  Action on fingerprint mismatch.

- progress:

  Logical (default \`FALSE\`).

## Value

A \`workflow_set\` object with populated result columns.

## See also

\[tune_grid_checkpointed()\], \[workflowsets::workflow_map()\]
