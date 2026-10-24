# Render One Report Template for Several Variants of Its Parameters

Renders a single parameterized Quarto (or R Markdown) report once per
variant, so that analyses which differ only in a choice (for example the
original data and the imputed data, `use_imputed = FALSE` / `TRUE`) are
produced from one canonical template instead of from copies that drift
apart. Each variant is a named list of parameters; the output files are
named `<template>_<variant>.<extension>`.

## Usage

``` r
render_variants(
  input,
  variants,
  formats = "html",
  output_dir = NULL,
  stop_on_error = TRUE,
  dry_run = FALSE,
  self_contained = TRUE,
  check_params = TRUE,
  renderer = NULL,
  quiet = TRUE
)
```

## Arguments

- input:

  Path to the `.qmd` or `.Rmd` template.

- variants:

  A named list; each element is a named list of parameters for one
  rendering, for example
  `list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE))`.
  Names must be unique and consist of letters, digits, `_`, `.` or `-`.

- formats:

  Output formats to render for each variant (default `"html"`), for
  example `c("html", "docx")`. Letters, digits, `_` and `-` only.

- output_dir:

  Folder for the outputs. Defaults to the folder of the template.

- stop_on_error:

  If `TRUE` (default), the first failed rendering stops the run with an
  error of class `render_variants_error` that carries the results so far
  in `$results`; if `FALSE`, failures are recorded in the result and the
  other variants are still rendered.

- dry_run:

  If `TRUE`, nothing is rendered; the plan is returned.

- self_contained:

  For `.qmd` HTML output, ask for one self-contained file; see Details.
  Default `TRUE`.

- check_params:

  If `TRUE` (default), check the variants' parameters against the
  template's `params:`; see Details.

- renderer:

  The function that renders one document, with the arguments of
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  (`input`, `output_format`, `output_file`, `execute_params`, `quiet`,
  `metadata`). Defaults to
  [`quarto::quarto_render`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  for a `.qmd` and to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  (with `params`) for an `.Rmd`.

- quiet:

  Passed on to the renderer.

## Value

A tibble with one row per variant and format: `variant`, `format`,
`params` (a list column), `output` (the final file), `status`
(`"rendered"`, `"failed"` or `"planned"`) and `error`.

## Details

**Parameters are checked.** Every parameter a variant sets must be
declared under `params:` in the template's YAML header: a misspelled
parameter is an error before anything is rendered (Quarto would
otherwise ignore it and silently render the default). The keys are read
from the header as written, so parameters such as `n`, `y` or `on` are
not mistaken for YAML booleans. A parameter that no variant sets keeps
the template's default. The check is skipped, with a warning, when the
header cannot be read; `check_params = FALSE` switches it off (needed
for a Jupyter notebook template whose parameters are declared in a
`parameters`-tagged cell).

**Your files are not touched.** Each rendering is written to a file with
a private name in the template's folder and then renamed or moved to its
final name, so a file that already exists next to the template (a
hand-made `analysis_final.html`, say) is never overwritten, moved or
deleted. A file at the final name *in `output_dir`* is replaced. The
private file is removed if the rendering fails.

**HTML.** Quarto's default HTML needs a folder of support files named
after the template, shared by every variant and not moved with the
output. `self_contained = TRUE` (default) therefore asks for a single
self-contained HTML file (`embed-resources: true`); with `FALSE` you
must keep the support folder yourself and variants overwrite each
other's figures. For an R Markdown template the setting is not applied
(`html_document` is self-contained by default).

## See also

\[render_pipeline()\], \[extract_yaml_formats()\]

## Examples

``` r
if (FALSE) { # \dontrun{
render_variants(
  "analysis/single_var_mixed_models.qmd",
  variants = list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE)),
  formats = c("html", "docx"),
  output_dir = "reports"
)
} # }
```
