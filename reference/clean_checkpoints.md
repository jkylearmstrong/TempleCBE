# Clean Up Mismatched or Superseded Checkpoint Files (E5)

Inspects \`.rds\` checkpoint files in \`dir\` and removes files from
older runs, mismatched fingerprints, or superseded \`seed_scheme\`
versions while keeping the active run's checkpoints.

## Usage

``` r
clean_checkpoints(
  dir,
  keep = c("current", "matching", "none"),
  run_fingerprint = NULL,
  seed_scheme = NULL,
  recursive = FALSE,
  dry_run = FALSE
)
```

## Arguments

- dir:

  Directory containing checkpoint \`.rds\` files.

- keep:

  Retention policy:

  \`"current"\`

  :   (Default) Keeps checkpoint files belonging to the most recent
      \`checkpoint_run_fingerprint\` in each checkpoint family (or the
      explicitly supplied \`run_fingerprint\`) and removes older,
      mismatched, or corrupted checkpoint files.

  \`"matching"\`

  :   Keeps only checkpoint files matching the supplied
      \`run_fingerprint\` and/or \`seed_scheme\`.

  \`"none"\`

  :   Removes all checkpoint files in \`dir\`.

- run_fingerprint:

  Optional expected \`checkpoint_run_fingerprint\` hash. When \`NULL\`
  under \`keep = "current"\`, the fingerprint of the newest valid
  checkpoint file per family (\`tune\` / \`score_outer\`) is used.

- seed_scheme:

  Optional expected seed scheme string (e.g. \`"v0.5.6"\`).

- recursive:

  Logical (default \`FALSE\`). Search subdirectories of \`dir\`.

- dry_run:

  Logical (default \`FALSE\`). When \`TRUE\`, reports which files would
  be kept or removed without deleting anything from disk.

## Value

A tibble of class \`cbe_clean_checkpoints\` with columns \`file\`,
\`path\`, \`family\`, \`run_fingerprint\`, \`seed_scheme\`, \`mtime\`,
\`action\` (\`"kept"\` or \`"removed"\`), and \`reason\`, plus
attributes \`n_kept\` and \`n_removed\`.

## See also

\[collect_checkpoints()\], \[tune_alpha_by_split()\], \[score_outer()\]
