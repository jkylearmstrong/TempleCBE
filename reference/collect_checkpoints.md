# Collect and Merge Sharded Checkpoints From \`checkpoint_dir\`

Merges per-task checkpoint files written by \[summarize_tune_results()\]
or \[score_outer()\] across multiple machines (\`shard = c(i, n)\`).
Verifies that all checkpoint files carry the same
\`checkpoint_run_fingerprint\` so results from different data, recipes,
feature sets, or settings cannot be combined by mistake.

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
  (\[summarize_tune_results()\]), or \`"score_outer"\`
  (\[score_outer()\]).

- allow_incomplete:

  Logical (default \`FALSE\`). When \`FALSE\`, stops with an error if
  any tasks from the full run grid are missing.

## Value

A merged tibble matching the return format of
\[summarize_tune_results()\] or \[score_outer()\].

## See also

\[summarize_tune_results()\], \[score_outer()\]
