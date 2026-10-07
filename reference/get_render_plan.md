# Get the topologically sorted list of QMDs that need to be re-rendered

Get the topologically sorted list of QMDs that need to be re-rendered

## Usage

``` r
get_render_plan(all_objects)
```

## Arguments

- all_objects:

  The full list of FilePath, FileUses, and FileOutputs objects.

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
