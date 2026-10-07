# Package Multiple Already-Rendered Reports Into an Indexed Zip

Given a data frame describing an ordered set of already-rendered Quarto
reports grouped into stage folders, copies their existing PDF/DOCX/HTML
outputs into a staged build directory, builds an Excel index with
hyperlinks to each report, optionally bundles arbitrary data-deliverable
files alongside them, and zips the result.

## Usage

``` r
zip_reports(
  reports,
  output_formats = c("pdf", "docx"),
  data_deliverables = character(0),
  zip_name = NULL,
  output_dir = getwd(),
  docx_from_pdf = NULL
)
```

## Arguments

- reports:

  A data frame/tibble with one row per report and columns `name`
  (display name), `path` (path to the `.qmd` source — the corresponding
  `.pdf`/`.docx`/`.html` output paths are derived by swapping its
  extension), and optionally `stage` (subfolder to group the report
  under; `NA`/empty becomes `"99_Other"`) and `description`. The stage
  is used as a folder name inside the zip, so path separators and the
  characters a file name cannot hold on Windows become `-` and leading
  or trailing dots, dashes and spaces are dropped (`"../x"` gives
  `"x"`); ordinary names such as `"01 Results"` are kept as they are.
  The index shows the stage as it was given.

- output_formats:

  Character vector of formats to look for and include: any of `"pdf"`,
  `"docx"`, `"html"`, or `"all"` for all three. Defaults to
  `c("pdf", "docx")`.

- data_deliverables:

  Character vector of additional file paths to copy into a top-level
  `data/` folder in the zip. Defaults to `character(0)`. A path that
  does not exist is skipped. Files that share a file name
  (`a/results.csv`, `b/results.csv`) are all kept, the later ones with a
  numeric suffix (`results_2.csv`) and a warning. A file that cannot be
  copied (open in another program, a folder instead of a file) is left
  out with a warning.

- zip_name:

  Name of the output zip file. Defaults to `all_reports_<date>.zip`.

- output_dir:

  Directory the finished zip is written to. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- docx_from_pdf:

  Optional function `function(src_pdf, dest_docx)` used to create
  `dest_docx` from an existing `src_pdf` when a report requests `"docx"`
  output but no (or a stale) `.docx` file exists next to its `.qmd`
  source. A DOCX is stale when it is older than its PDF. If `NULL`
  (default), a missing DOCX is skipped silently and a stale one is
  skipped with a warning; it is never shipped. The function counts as
  having failed when it returns `FALSE` (as
  [`convert_pdf_to_docx`](https://jkylearmstrong.github.io/TempleCBE/reference/convert_pdf_to_docx.md)
  does) or when no DOCX at least as new as the PDF is there afterwards;
  a warning says so and no DOCX is shipped for that report. Any other
  return value is taken as success. In the index, the DOCX cell of a
  report whose PDF exists but that has no DOCX to ship reads
  `"not converted"`.

## Value

The path to the created zip file.

## Details

Unlike
[`zip_render`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_render.md)
(which renders and zips a single document), `zip_reports` does not
render anything itself — it packages outputs that have already been
rendered, e.g. by
[`render`](https://jkylearmstrong.github.io/TempleCBE/reference/render.md)
(or
[`render_me`](https://jkylearmstrong.github.io/TempleCBE/reference/render.md)).
Report order and staging is entirely the caller's decision (for example,
the topological sort of a project's own dependency graph) — pass
`reports` pre-ordered. If multiple reports share the same source stem
(for example `analysis1/analysis.qmd` and `analysis2/analysis.qmd`),
staged output names are disambiguated to prevent overwrites within a
stage/format folder.

## Examples

``` r
if (FALSE) { # \dontrun{
reports <- data.frame(
  name = c("Introduction", "Results"),
  path = c("analysis/intro.qmd", "analysis/results.qmd"),
  stage = c("00_intro", "01_results"),
  description = c("Project overview", "Primary analysis results")
)
zip_reports(reports, output_formats = c("pdf", "docx"))
} # }
```
