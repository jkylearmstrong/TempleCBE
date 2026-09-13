# Install the Temple Brand Quarto Extension Into a Project

Installs
[quarto_temple_brand](https://github.com/jkylearmstrong-temple/quarto_temple_brand)
with
[`quarto::quarto_add_extension()`](https://quarto-dev.github.io/quarto-r/reference/quarto_add_extension.html),
creating a minimal `_quarto.yml` first if there isn't one (Quarto only
applies a brand extension inside a project). Documents in `path` can
then use `format: temple-html`, `temple-pdf` (LaTeX title page),
`temple-typst`, or `temple-revealjs`, and every format picks up the
brand colors, fonts, and logo.

## Usage

``` r
use_temple_brand(
  path = ".",
  extension = "jkylearmstrong-temple/quarto_temple_brand",
  quiet = FALSE
)
```

## Arguments

- path:

  Project directory (created if missing). Defaults to the working
  directory.

- extension:

  Extension source passed to `quarto add`: a GitHub `org/repo`, a URL,
  or a local path.

- quiet:

  Logical; suppress Quarto's output.

## Value

Invisibly, a list with `path`, `quarto_yml`, `created_quarto_yml`, and
`extension_dir`.

## See also

[`create_report`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)
with `template_name = "temple"`.

## Examples

``` r
if (FALSE) { # \dontrun{
use_temple_brand("analysis")
create_report("analysis", template_name = "temple")
} # }
```
