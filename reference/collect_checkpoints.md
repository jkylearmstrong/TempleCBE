# Collect and Merge Sharded Checkpoints From \`checkpoint_dir\`

Merges per-task checkpoint files written by \[tune_alpha_by_split()\] or
\[score_outer()\] across multiple machines (\`shard = c(i, n)\`).
Verifies that all checkpoint files carry the same
\`checkpoint_run_fingerprint\` and \`seed_scheme\` so results from
different data, recipes, feature sets, seed schemes, or settings cannot
be combined by mistake.

## Usage

``` r
collect_checkpoints(
  checkpoint_dir,
  type = c("auto", "tune", "score_outer"),
  allow_incomplete = FALSE
)
```

## Arguments

- checkpoint_dir:

  Directory containing \`.rds\` checkpoint files.

- type:

  Optional checkpoint type to collect: \`"auto"\` (default; inferred
  from files in \`checkpoint_dir\`), \`"tune"\`
  (\[tune_alpha_by_split()\]), or \`"score_outer"\` (\[score_outer()\]).

- allow_incomplete:

  Logical (default \`FALSE\`). When \`FALSE\`, stops with an error if
  any tasks from the full run grid are missing.

## Value

A merged tibble matching the return format of \[tune_alpha_by_split()\]
or \[score_outer()\].

## See also

\[tune_alpha_by_split()\], \[score_outer()\], \[clean_checkpoints()\]
