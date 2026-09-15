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
