# Scaffold a New Report From a Template

Copies a bundled report template (and its supporting bibliography/title
files) into `location`.

## Usage

``` r
create_report(
  location = getwd(),
  template_name = "t_test_example",
  child = TRUE,
  type = ".qmd",
  include_bib = TRUE,
  include_tex = TRUE,
  install_brand = FALSE,
  filename = NULL
)
```

## Arguments

- location:

  Directory to create the report in (default
  [`getwd()`](https://rdrr.io/r/base/getwd.html)).

- template_name:

  One of `"t_test_example"` (default), `"example"`, or `"temple"`.

- child:

  Logical (default `TRUE`); also copy the child-document template.

- type:

  One of `".qmd"` (default) or `".Rmd"`.

- include_bib:

  Logical (default `TRUE`); also copy the `.bib` file.

- include_tex:

  Logical (default `TRUE`); also copy the title `.tex` file.

- install_brand:

  Logical (default `FALSE`); for the `"temple"` template, also run
  [`use_temple_brand`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)`(location)`,
  which downloads the extension into `location` itself. Leave this
  `FALSE` when the extension is already installed at the project root
  (see
  [`use_temple_brand`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md));
  it doesn't need reinstalling per report.

- filename:

  Base name (no extension) for the report file, default `NULL` uses
  `template_name`. The report is always written to `location`, so
  calling `create_report()` twice for the same `location` with the same
  `template_name` (or the same `filename`) overwrites the first report;
  pass a distinct `filename` for each report that shares a `location`,
  e.g. `create_report("analysis", filename = "analysis1")` and
  `create_report("analysis", filename = "analysis2")`.

## Value

A list indicating whether each file was created.

## Details

The `"temple"` template renders with the
[quarto_temple_brand](https://github.com/jkylearmstrong-temple/quarto_temple_brand)
extension's `temple-html`, `temple-pdf`, and `temple-typst` formats, so
it needs that extension installed somewhere Quarto can find it from
`location` (see
[`use_temple_brand`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)):
either at an ancestor project root shared by every report, or, with
`install_brand = TRUE`, in `location` itself for a one-off report. It
uses no `title.tex` or child document, and is Quarto-only.

## Examples

``` r
if (FALSE) { # \dontrun{
create_report(here::here("analysis"))

# A project with several "temple" reports: install the extension once at
# the project root, then scaffold each report without install_brand.
use_temple_brand(here::here())
create_report(here::here("analysis"), template_name = "temple")
create_report(here::here("reports"), template_name = "temple", filename = "q3")

# A single one-off "temple" report instead installs beside itself.
create_report(here::here("analysis"), template_name = "temple", install_brand = TRUE)

# Two reports sharing one location need distinct `filename`s, or the
# second call overwrites the first report file:
create_report(here::here("analysis"), template_name = "temple", filename = "analysis1")
create_report(here::here("analysis"), template_name = "temple", filename = "analysis2")
} # }
```
