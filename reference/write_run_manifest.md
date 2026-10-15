# Write a Run Manifest JSON Beside Render Outputs

Records environment metadata, git commit hash, package versions, random
number generator seed, execution future plan, and per-node wall times
beside render outputs.

## Usage

``` r
write_run_manifest(output_dir, targets_result, seed = NULL)
```

## Arguments

- output_dir:

  Directory to write \`render_manifest.json\` into.

- targets_result:

  Data frame of render timing and status from \[render()\].

- seed:

  Optional integer RNG seed used for the run.

## Value

The absolute path to the written \`render_manifest.json\` file.
