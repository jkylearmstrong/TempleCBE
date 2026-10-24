# Generate a Self-Rendering Reproducibility Appendix (E2)

Collects and renders a standardized reproducibility record for Quarto
and R Markdown reports, including:

1.  Package versions and R session platform metadata

2.  Git commit SHA and working tree status

3.  RNG seed and task \`seed_scheme\` version (\`"v0.5.6"\`)

4.  Active \`future::plan()\` and worker count

5.  SHA-256 content hashes of input datasets or pipeline hash manifests

6.  Pre-flight \[validate_start_stop()\] summary

7.  Checkpoint directory fingerprints and \`seed_scheme\` audit

8.  Render manifest (\`render_manifest.json\`) summary

## Usage

``` r
repro_appendix(
  run = NULL,
  data = NULL,
  validation = NULL,
  run_dir = NULL,
  manifest_path = NULL,
  seed = NULL,
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  packages = c("TempleCBE", "survival", "glmnet", "recipes", "rsample", "yardstick",
    "furrr", "future", "quarto", "rmarkdown", "renv"),
  title = "Reproducibility Appendix",
  heading_level = 2L,
  format = c("markdown", "list")
)
```

## Arguments

- run:

  Optional \[nested_survival_run()\] object (or list of such objects
  across domains). When supplied, \`data\`, \`validation\`, \`run_dir\`,
  \`seed\`, and \`checkpoint_fingerprints\` are extracted from \`run\`
  automatically.

- data:

  Optional data frame or named list of data frames to hash with SHA-256
  (and validate with \[validate_start_stop()\] if \`validation\` is not
  supplied).

- validation:

  Optional \[validate_start_stop()\] result.

- run_dir:

  Optional directory path containing checkpoints and/or
  \`render_manifest.json\`.

- manifest_path:

  Optional explicit path to \`render_manifest.json\`.

- seed:

  Optional integer seed used for the report or run.

- id, start, stop, status:

  Column names passed to \[validate_start_stop()\] when \`data\` is a
  survival data frame and \`validation\` is \`NULL\`.

- packages:

  Character vector of packages whose versions should be recorded.

- title:

  Section heading for the appendix (default \`"Reproducibility
  Appendix"\`).

- heading_level:

  Integer Markdown heading level (default \`2L\`, i.e. \`##\`).

- format:

  Output representation: \`"markdown"\` (default; returns a character
  object of class \`c("cbe_repro_appendix", "knit_asis")\` with all
  structured metadata attached in attributes and \`\$\` accessors) or
  \`"list"\` (returns the structured list directly).

## Value

An object of class \`cbe_repro_appendix\` containing the rendered
Markdown string and structured fields \`package_versions\`, \`git\`,
\`seed\`, \`seed_scheme\`, \`future_plan\`, \`data_hashes\`,
\`validation\`, \`checkpoint_fingerprints\`, \`render_manifest\`, and
\`markdown\`.

## Details

Can be called directly inside a report chunk (where its \`knit_print\`
and \`print\` methods emit ready-to-render Markdown), or referenced via
the packaged Quarto child template returned by
\[repro_appendix_template()\].

## See also

\[repro_appendix_template()\], \[write_run_manifest()\],
\[validate_start_stop()\], \[nested_survival_run()\]
