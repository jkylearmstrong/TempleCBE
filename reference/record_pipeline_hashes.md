# Record or Update Content Hashes in \`pipeline_hashes.json\`

Computes SHA-256 content hashes for all declared input files, producer
scripts, and existing output deliverables in a pipeline and writes them
to \`pipeline_hashes.json\`. Used by \`render(stale_by = "hash")\` and
\`is_stale(stale_by = "hash")\` so reruns across machines or git
checkouts only re-render reports whose inputs or scripts genuinely
changed in content.

## Usage

``` r
record_pipeline_hashes(all_objects, hash_file = NULL)
```

## Arguments

- all_objects:

  A list of \[FilePath\], \[FileUses\], and \[FileOutputs\] objects (or
  a single pipeline object).

- hash_file:

  Optional file path for the JSON hash store. Defaults to
  \`"pipeline_hashes.json"\` in the pipeline directory.

## Value

The path to the written \`pipeline_hashes.json\` file (invisibly).

## See also

\[is_stale()\], \[get_render_plan()\], \[render()\]
