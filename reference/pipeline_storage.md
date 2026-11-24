# Save and Load Computational Pipeline Graph

Serializes a computational pipeline object list to an RDS file along
with metadata, and reloads it while ensuring all node and file
modification times (\`mtime\`) are accurately refreshed from disk. This
replaces static, unmanaged pipeline dumps that become stale over time.

## Usage

``` r
save_pipeline(pipeline, file = "pipeline.rds", root = NULL)

load_pipeline(
  file = "pipeline.rds",
  update_mtime = TRUE,
  root = NULL,
  from = NULL
)
```

## Arguments

- pipeline:

  A list of pipeline objects (\[FilePath\], \[FileOutputs\],
  \[FileUses\]).

- file:

  Path to the \`.rds\` file to save to or read from. \`load_pipeline()\`
  reads it with \[readRDS()\], which can run code stored in the file:
  load only files written by \`save_pipeline()\` in a location that you
  trust.

- root:

  For \`save_pipeline()\`: optional project root (such as
  \`here::here()\`). Paths under it are stored relative to it, so the
  saved pipeline can be loaded on another machine, or another drive,
  where the project lives somewhere else (\`S:/...\` on Windows,
  \`/home/...\` on Linux). For \`load_pipeline()\`: the root to resolve
  those relative paths against; the default is \`here::here()\` on the
  machine that loads it.

- update_mtime:

  Logical. When loading, should the \`@mtime\` slots of all contained
  files be re-read from disk? Defaults to \`TRUE\`. Every file is
  re-read, including dependencies of dependencies.

- from:

  For \`load_pipeline()\`: optional old project root(s) of a pipeline
  saved with absolute paths (for example \`"S:/Clinical
  Science/Staff/project"\`); paths under them are moved under \`root\`.
  See \[relocate_pipeline()\].

## Value

\`save_pipeline\` returns \`file\` invisibly on success.
\`load_pipeline\` returns the list of pipeline objects with refreshed
timestamps.

## See also

\[relocate_pipeline()\], \[relativize_pipeline()\]
