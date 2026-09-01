# Render a Quarto Document and Zip It With Its Dependencies

Renders `input` in an isolated build directory, copies in any resources
it references (explicitly, or heuristically detected from quoted paths /
[`here::here()`](https://here.r-lib.org/reference/here.html) calls), and
zips the outputs together with the source and sidecar files.

## Usage

``` r
zip_render(
  input,
  formats = c("html", "pdf", "docx"),
  resources = NULL,
  detect = c("heuristic", "none"),
  build_dir = NULL,
  zip_name = NULL,
  copy_back_dir = NULL,
  include_sources = TRUE,
  overwrite = TRUE,
  verbose = TRUE
)
```

## Arguments

- input:

  Path to the input `.qmd` file.

- formats:

  Character vector of output formats (e.g. `c("html","pdf","docx")` or
  `"all"`).

- resources:

  Optional character vector of extra files to include, absolute or
  project-relative.

- detect:

  `"heuristic"` (default; scans the `.qmd` for likely file paths) or
  `"none"`.

- build_dir:

  Staging directory; defaults to a fresh temp directory.

- zip_name:

  Name of the resulting zip; defaults to `<input-stem>.zip`.

- copy_back_dir:

  Where to copy the finished zip; defaults to `dirname(input)`.

- include_sources:

  Logical (default `TRUE`); include the `.qmd` and sidecar bib/tex/css
  files.

- overwrite:

  Logical (default `TRUE`); overwrite an existing zip at the
  destination.

- verbose:

  Logical (default `TRUE`); print progress messages.

## Value

Invisibly, a list with the build directory, detected/copied resources,
render outputs, and final zip path.

## Examples

``` r
if (FALSE) { # \dontrun{
zip_render("report.qmd", formats = c("html", "pdf"))
} # }
```
