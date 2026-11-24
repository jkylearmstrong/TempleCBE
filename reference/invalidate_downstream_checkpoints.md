# Delete the Checkpoints Downstream of a Changed Node

When one document or data file of a pipeline changes, only what depends
on it needs to be recomputed. This finds every node reachable from
\`modified_node\` along the dependency edges of the pipeline (\`graph\`,
the list of \[FilePath\], \[FileUses\] and \[FileOutputs\] objects), and
deletes the checkpoint files in \`checkpoint_dir\` that belong to those
nodes. Files the modified node produces are downstream and go too;
checkpoints named after the modified node itself are kept unless
\`include_self = TRUE\`; checkpoints of unrelated nodes are never
touched.

## Usage

``` r
invalidate_downstream_checkpoints(
  graph,
  modified_node,
  checkpoint_dir = "checkpoints/",
  include_self = FALSE,
  pattern = "\\.rds$",
  dry_run = FALSE
)
```

## Arguments

- graph:

  A list of pipeline objects (\[FilePath\], \[FileUses\],
  \[FileOutputs\]).

- modified_node:

  Name or path of the node that changed.

- checkpoint_dir:

  Folder with the checkpoint files.

- include_self:

  Also remove the modified node's own checkpoints (default \`FALSE\`).

- pattern:

  Regular expression for files that count as checkpoints (default
  \`"\\rds\$"\`).

- dry_run:

  List what would be removed without removing it.

## Value

A tibble with \`node\` and \`file\` (relative to \`checkpoint_dir\`) and
\`removed\`, invisibly.

## Details

A checkpoint belongs to a node when its path (relative to
\`checkpoint_dir\`, without extension) starts with the node's \`name\`
or with the file name stem of its \`path\`, either as a file
(\`\<stem\>.rds\`, \`\<stem\>\_\<key\>.rds\`) or inside a folder of that
name (\`\<stem\>/...\`); the match is on whole words, so \`fit\` does
not match \`fit2.rds\`. Files a node declares as outputs that lie inside
\`checkpoint_dir\` belong to it as well.

## See also

\[checkpoint_map()\], \[clean_checkpoints()\], \[get_render_plan()\]

## Examples

``` r
raw <- FilePath("raw", "data/raw.csv")
clean <- FileOutputs("clean", "01_clean.qmd", dependencies = list(raw),
                     output = list(FilePath("clean_rds", "clean.rds")))
model <- FileOutputs("model", "02_model.qmd", dependencies = list(FilePath("clean_rds", "clean.rds")),
                     output = list(FilePath("model_rds", "model.rds")))
ck <- file.path(tempdir(), "ck_demo"); dir.create(ck, showWarnings = FALSE)
file.create(file.path(ck, c("clean.rds", "model.rds", "other.rds")))
#> [1] TRUE TRUE TRUE
invalidate_downstream_checkpoints(list(raw, clean, model), "clean", ck, dry_run = TRUE)
```
