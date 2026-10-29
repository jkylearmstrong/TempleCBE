# Checkpointed Survival Racing Optimization (\`tune_race_survival_checkpointed\`)

A resumable wrapper around \[tune_race_survival()\] with fingerprinted
disk caching.

## Usage

``` r
tune_race_survival_checkpointed(
  object,
  resamples,
  ...,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error"),
  progress = FALSE
)
```

## Arguments

- object:

  A workflow or workflow set.

- resamples:

  An \`rset\` resampling object.

- ...:

  Arguments passed to \[tune_race_survival()\].

- checkpoint_dir:

  Directory to store checkpoints.

- shard:

  Optional shard specification.

- checkpoint_mismatch:

  Mismatch handling action.

- progress:

  Logical (default \`FALSE\`).

## Value

A racing tuning object.

## See also

\[tune_race_survival()\], \[tune_grid_checkpointed()\]
