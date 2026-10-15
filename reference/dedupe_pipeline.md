# Deduplicate Pipeline Objects by Name

Resolves duplicate object names in a pipeline object list resulting from
merged lists or overlapping definitions.

## Usage

``` r
dedupe_pipeline(
  all_objects,
  strategy = c("keep_first", "prefer_outputs", "keep_last", "rename"),
  verbose = FALSE
)
```

## Arguments

- all_objects:

  List of \`FilePath\`, \`FileUses\`, or \`FileOutputs\` objects.

- strategy:

  Conflict resolution strategy:

  - `"keep_first"` (default): Preserves the first object encountered for
    each name.

  - `"prefer_outputs"`: Prefers `FileOutputs` over `FileUses`, and
    `FileUses` over `FilePath`.

  - `"keep_last"`: Preserves the last object encountered for each name.

  - `"rename"`: Preserves all objects, appending suffixes `"_2"`,
    `"_3"`, etc. to duplicates.

- verbose:

  Logical. When `TRUE`, messages the names and resolution of
  deduplicated objects.

## Value

A deduplicated list of pipeline objects.
