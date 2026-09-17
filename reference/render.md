# Render Quarto and R Markdown Documents to Multiple Formats With Timing

Renders one or more Quarto (`.qmd`) or R Markdown (`.Rmd`) documents to
multiple output formats (by default both PDF and DOCX, or formats
defined in YAML or compute graphs) sequentially or in parallel,
recording execution duration for each document/format combination.
Useful for workflows where reviewers need a Word (`.docx`) file for
track changes/edits while publication and archival copies need PDF.

## Usage

``` r
render(
  path,
  formats = getOption("pipeline.default_formats", c("pdf", "docx")),
  pattern = "\\.(qmd|Rmd|rmd)$",
  engine = c("auto", "quarto", "rmarkdown"),
  workers = NULL,
  ...
)

render_me(
  path,
  formats = getOption("pipeline.default_formats", c("pdf", "docx")),
  pattern = "\\.(qmd|Rmd|rmd)$",
  engine = c("auto", "quarto", "rmarkdown"),
  workers = NULL,
  ...
)
```

## Arguments

- path:

  Target document(s) to render. Can be:

  - A character string specifying a file path or directory.

  - A character vector of file paths.

  - A list of file paths (e.g. `list("report1.qmd", "report2.Rmd")`).

  - An S4 compute graph object (`FileOutputs` or `FilePath`).

  - A list of compute graph objects, such as a topological render plan
    returned by
    [`get_render_plan`](https://jkylearmstrong.github.io/TempleCBE/reference/get_render_plan.md)
    or a pipeline list from
    [`pipeline_config`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_config.md).

  - A named list or list of lists specifying per-document paths and
    formats (e.g.
    `list("report1.qmd" = c("pdf", "docx"), "report2.Rmd" = "html")`).

- formats:

  Output format(s) to generate. Defaults to
  `getOption("pipeline.default_formats", c("pdf", "docx"))`. Can be:

  - A character vector of formats (e.g. `c("pdf", "docx")`, `"html"`,
    `"gfm"`).

  - `"yaml"` or `"auto"` (or `NULL`) to read and render the output
    formats declared in each document's own YAML frontmatter.

  - Standard shorthand formats (`"pdf"`, `"docx"`, `"html"`, `"gfm"`)
    are automatically translated to R Markdown equivalents
    (`"pdf_document"`, `"word_document"`, `"html_document"`,
    `"github_document"`) when rendering with rmarkdown.

  When a document in a compute graph defines its own deliverable output
  formats (via
  [`create_qmd_renderer`](https://jkylearmstrong.github.io/TempleCBE/reference/create_qmd_renderer.md)),
  those document-level formats override the default `formats`.

- pattern:

  Optional regular expression to filter files when `path` is a
  directory. Defaults to `"\.(qmd|Rmd|rmd)$"`.

- engine:

  Rendering engine: `"auto"` (default), `"quarto"`, or `"rmarkdown"`.
  When `"auto"`, `.qmd` files are rendered via quarto, and `.Rmd` files
  are rendered via quarto if available or rmarkdown otherwise.

- workers:

  Integer specifying the number of multisession parallel workers when
  rendering multiple documents. Defaults to `NULL`, which caps at
  available cores minus one. Ignored (rendering falls back to
  sequential) when the future/furrr packages are not installed.

- ...:

  Additional options passed directly to the rendering backend
  ([`quarto::quarto_render`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  or
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)),
  such as `params`, `execute_params`, `output_dir`, `clean`, or `quiet`.

## Value

A data frame containing:

- file:

  Base filename of the rendered document

- output:

  Output format (e.g., "pdf", "docx")

- duration:

  Execution time in seconds

- status:

  "success" or error condition message

## Examples

``` r
if (FALSE) { # \dontrun{
# Render a single Quarto report to both PDF and DOCX
render(here::here("analysis", "report.qmd"))

# Render a single R Markdown report to both PDF and Word
render(here::here("analysis", "report.Rmd"))

# Render using formats declared in document's own YAML
render("analysis/report.qmd", formats = "yaml")

# Render a compute graph render plan
# plan <- get_render_plan(pipeline)
# render(plan)

# Render all QMD and Rmd reports in a directory in parallel
render("analysis/")
} # }
```
