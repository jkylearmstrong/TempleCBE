# Write a Run Manifest JSON Beside Render Outputs

Records environment metadata, git commit hash, package versions, random
number generator seed, execution future plan, and per-node wall times
beside render outputs.

## Usage

``` r
write_run_manifest(
  output_dir,
  targets_result,
  seed = NULL,
  seed_scheme = current_seed_scheme()
)
```

## Arguments

- output_dir:

  Directory to write \`render_manifest.json\` into.

- targets_result:

  Data frame of render timing and status from \[render_pipeline()\].

- seed:

  Optional integer RNG seed used for the run.

- seed_scheme:

  Task seed generation scheme identifier (defaults to
  \`current_seed_scheme()\`, \`"v0.5.6"\`).

## Value

The absolute path to the written \`render_manifest.json\` file.
