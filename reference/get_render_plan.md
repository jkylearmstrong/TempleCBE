# Get the topologically sorted list of QMDs that need to be re-rendered

Get the topologically sorted list of QMDs that need to be re-rendered

## Usage

``` r
get_render_plan(
  all_objects,
  only_stale = TRUE,
  from = NULL,
  to = NULL,
  dry_run = FALSE
)
```

## Arguments

- all_objects:

  The full list of FilePath, FileUses, and FileOutputs objects.

- only_stale:

  Logical. When \`TRUE\` (default), plans only documents that are out of
  date or affected by upstream changes. When \`FALSE\`, plans all
  renderable nodes in topological dependency order (for full pipeline
  reruns).

- from:

  Optional node name or 1-based integer index in the topological plan to
  start execution from.

- to:

  Optional node name or 1-based integer index in the topological plan to
  end execution at.

- dry_run:

  Logical. When \`TRUE\`, prints the ordered plan to console without
  executing and returns the planned objects invisibly.

## Value

A list of FileOutputs objects (the QMDs) in the correct render order.

## Details

The graph's identity is the node `name`, not the file `path`: a file
declared under two names is two nodes, so a report that reads it under
one name is not planned when the report that writes it under the other
is. This is reported with a warning, as is one name used for two files,
and so are two producers that declare the same output file. Two objects
of `all_objects` with the same name are an error. See
[`as_igraph`](https://jkylearmstrong.github.io/TempleCBE/reference/as_igraph.md).
