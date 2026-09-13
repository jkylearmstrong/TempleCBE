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
  install_brand = FALSE
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
  which downloads the extension.

## Value

A list indicating whether each file was created.

## Details

The `"temple"` template renders with the
[quarto_temple_brand](https://github.com/jkylearmstrong/quarto_temple_brand)
extension's `temple-html`, `temple-pdf`, and `temple-typst` formats, so
it needs that extension installed beside it (see
[`use_temple_brand`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)).
It uses no `title.tex` or child document, and is Quarto-only.

## Examples

``` r
if (FALSE) { # \dontrun{
create_report(here::here("analysis"))
create_report(here::here("analysis"), template_name = "temple", install_brand = TRUE)
} # }
```
