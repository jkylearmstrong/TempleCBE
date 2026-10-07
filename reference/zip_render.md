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

  Staging directory; defaults to a fresh temp directory. A relative path
  is relative to the working directory the function is called from.

- zip_name:

  Name of the resulting zip; defaults to `<input-stem>.zip`.

- copy_back_dir:

  Where to copy the finished zip; defaults to `dirname(input)`. A
  relative path is relative to the working directory the function is
  called from.

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

## Details

The rendered outputs are the files in the build directory named
`<input-stem>.<extension>` for the extensions of `formats` (the case of
the name is ignored). If there are none, for instance because the format
writes its output to another folder, a warning says so and the zip holds
the sources only.

Heuristic detection inspects quoted file paths and here::here() calls in
the document. For YAML front-matter resources, pass them explicitly via
the \`resources\` argument or include them in the document's YAML; this
function will attempt to parse YAML when present to pick up top-level
resource lists.

## Examples

``` r
if (FALSE) { # \dontrun{
zip_render("report.qmd", formats = c("html", "pdf"))
} # }
```
