# Render Pipeline Documents and Reports

Dispatches Quarto or R Markdown compilation across single documents,
directories, or computational pipeline graphs, with support for
staleness detection, range filtering, cache invalidation, error
recovery, run manifests, and renv guards.

## Usage

``` r
render(
  path,
  formats = getOption("pipeline.default_formats", c("pdf", "docx")),
  pattern = "\\.(qmd|Rmd|rmd)$",
  engine = c("auto", "quarto", "rmarkdown"),
  workers = 1,
  only_stale = TRUE,
  from = NULL,
  to = NULL,
  dry_run = FALSE,
  cache = c("keep", "refresh", "stale"),
  on_error = c("continue", "stop"),
  resume = NULL,
  manifest = TRUE,
  renv_guard = TRUE,
  seed = NULL,
  skip_downstream = TRUE,
  rerun = c("none", "full"),
  ...
)

render_me(
  path,
  formats = getOption("pipeline.default_formats", c("pdf", "docx")),
  pattern = "\\.(qmd|Rmd|rmd)$",
  engine = c("auto", "quarto", "rmarkdown"),
  workers = 1,
  only_stale = TRUE,
  from = NULL,
  to = NULL,
  dry_run = FALSE,
  cache = c("keep", "refresh", "stale"),
  on_error = c("continue", "stop"),
  resume = NULL,
  manifest = TRUE,
  renv_guard = TRUE,
  seed = NULL,
  skip_downstream = TRUE,
  rerun = c("none", "full"),
  ...
)
```

## Arguments

- path:

  A file path (character), directory, list of files, or list of pipeline
  objects (\`FilePath\`, \`FileUses\`, \`FileOutputs\`).

- formats:

  Character vector of output formats (e.g. \`c("pdf", "docx",
  "html")\`).

- pattern:

  File pattern regex matching documents to render.

- engine:

  Rendering engine: \`"auto"\` (default), \`"quarto"\`, or
  \`"rmarkdown"\`.

- workers:

  Number of parallel workers for concurrent rendering via
  \`future\`/\`furrr\`. Defaults to \`1\` (sequential). Set to \`\> 1\`
  to initialize multisession workers, or \`NULL\` to use an existing
  \`future::plan()\` without altering it.

- only_stale:

  Logical. When \`TRUE\` (default), only out-of-date documents are
  rendered. When \`FALSE\`, performs a full graph rerun.

- from:

  Optional starting node name or index in the topological plan.

- to:

  Optional ending node name or index in the topological plan.

- dry_run:

  Logical. When \`TRUE\`, prints the ordered render plan without
  executing.

- cache:

  Cache handling: \`"keep"\` (default), \`"refresh"\` (wipes \`\_cache\`
  and \`\_files\`), or \`"stale"\`.

- on_error:

  Behavior upon error: \`"continue"\` (default) or \`"stop"\`.

- resume:

  Optional data frame of previous render results to resume from.

- manifest:

  Logical. When `TRUE` (default), writes a `render_manifest.json` beside
  rendered outputs containing TempleCBE version, git commit hash,
  package versions, RNG seed, future plan, and per-document wall time.

- renv_guard:

  Logical. When `TRUE` (default), verifies that renv is active if an
  `renv.lock` exists, and warns of any package version discrepancies.

- seed:

  Optional integer seed for reproducible document execution.

- skip_downstream:

  Logical. When \`TRUE\` (default), if an upstream node fails during
  pipeline rendering (with \`on_error = "continue"\`), all dependent
  downstream reports are skipped and marked with status
  \`"skipped_upstream_failure"\` to prevent compiling on stale data.

- rerun:

  Rerun strategy shorthand: \`"none"\` (default, uses \`only_stale\` and
  \`cache\` settings) or \`"full"\` (forces full graph execution with
  fresh caches, setting \`only_stale = FALSE\` and \`cache =
  "refresh"\`).

- ...:

  Additional arguments passed to rendering engines.

## Value

A data frame of rendered files, output formats, durations, and status.
