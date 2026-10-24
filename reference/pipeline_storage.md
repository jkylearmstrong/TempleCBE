# Save and Load Computational Pipeline Graph

Serializes a computational pipeline object list to an RDS file along
with metadata, and reloads it while ensuring all node and file
modification times (\`mtime\`) are accurately refreshed from disk. This
replaces static, unmanaged pipeline dumps that become stale over time.

## Usage

``` r
save_pipeline(pipeline, file = "pipeline.rds")

load_pipeline(file = "pipeline.rds", update_mtime = TRUE)
```

## Arguments

- pipeline:

  A list of pipeline objects (\[FilePath\], \[FileOutputs\],
  \[FileUses\]).

- file:

  Path to the \`.rds\` file to save to or read from.

- update_mtime:

  Logical. When loading, should the \`@mtime\` slots of all contained
  files be re-read from disk? Defaults to \`TRUE\`.

## Value

\`save_pipeline\` returns \`file\` invisibly on success.
\`load_pipeline\` returns the list of pipeline objects with refreshed
timestamps.
