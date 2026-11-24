# Shrink Existing Checkpoint Files by Dropping Note Backtraces

Checkpoints written by TempleCBE 0.11.1 and earlier keep the rlang
backtraces that tune (2.0.0 and later) stores with each note in a tuning
result's \`.notes\` column. A backtrace can reach the environments of
the frames that raised a warning, so each saved note can carry its own
copy of the training data, and a workflow's checkpoint can be many times
the size of its metrics and predictions. Since 0.11.2 new checkpoints
are written without them (see \`options(templecbe.keep_note_traces)\`);
\`compact_checkpoints()\` rewrites checkpoints that are already on disk
the same way, so a long run does not have to be recomputed to get the
smaller files.

## Usage

``` r
compact_checkpoints(dir, recursive = TRUE, dry_run = FALSE)
```

## Arguments

- dir:

  An existing checkpoint directory, such as the \`checkpoint_dir\` of
  \[workflow_map_checkpointed()\], \[tune_grid_checkpointed()\] or
  \[fit_resamples_checkpointed()\].

- recursive:

  Logical (default \`TRUE\`). Also rewrite the files in the
  subdirectories of \`dir\` (\[workflow_map_checkpointed()\] keeps one
  subdirectory per workflow).

- dry_run:

  Logical (default \`FALSE\`). When \`TRUE\`, only report the sizes the
  files would have, without changing them.

## Value

A tibble, one row per \`.rds\` file, with columns \`path\`,
\`size_before\` and \`size_after\` (bytes) and \`action\`
(\`"compacted"\`, \`"would compact"\`, \`"unchanged"\` or
\`"unreadable"\`), invisibly. A message reports the total saving.

## Details

Each \`.rds\` file in \`dir\` is read, the backtraces of the tuning
results it holds are set to \`NULL\` (keeping the note text, its type
and location, and every fingerprint and attribute), and the file is
replaced only when the new one is smaller. The new file is written next
to the old one and renamed over it, so an interrupted run leaves either
the old file or the new one. A file that holds no backtrace, or cannot
be read, is left alone. A file is read with \[readRDS()\], which can run
code stored in it: only point \`dir\` at checkpoints that you (or a
process you trust) wrote.

## See also

\[clean_checkpoints()\], \[collect_checkpoints()\]

## Examples

``` r
# \donttest{
tmp <- file.path(tempdir(), "compact_demo")
dir.create(tmp)
saveRDS(list(a = 1), file.path(tmp, "ckpt_a.rds"))
compact_checkpoints(tmp, dry_run = TRUE)
#> compact_checkpoints(): would compact 0 of 1 file(s) in '/tmp/Rtmpn0GH9K/compact_demo', which would save 0.0 B.
unlink(tmp, recursive = TRUE)
# }
```
