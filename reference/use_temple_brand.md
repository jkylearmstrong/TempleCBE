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
  quiet = FALSE,
  check_root = TRUE
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

- check_root:

  Logical; warn when `path` isn't the current project's root as found by
  [`here`](https://here.r-lib.org/reference/here.html) (i.e. the nearest
  ancestor with a `.Rproj`, `.git`, or similar marker). Set to `FALSE`
  to install into a report's own subfolder without the warning, e.g. for
  a one-off report that won't grow siblings.

## Value

Invisibly, a list with `path`, `quarto_yml`, `created_quarto_yml`, and
`extension_dir`.

## Details

**Install once, at the project root.** Quarto resolves a project's
`_extensions` from any subfolder of that project, so a single install at
the root (where `_quarto.yml` lives) covers every report under it.
Calling `use_temple_brand()` again for each report's own subfolder
instead creates a separate `_extensions` copy per report, which can
drift out of sync as the extension is updated. See
[`create_report`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)'s
`install_brand` argument for the one-report shortcut, which installs
into that report's own folder and is fine when there's only ever going
to be one.

## See also

[`create_report`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)
with `template_name = "temple"`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install once at the project root; every report below it shares the
# extension automatically.
use_temple_brand(here::here())
create_report(here::here("analysis"), template_name = "temple")
create_report(here::here("reports"), template_name = "temple", filename = "q3")

# A single, one-off report: install alongside it and skip the check.
use_temple_brand("analysis", check_root = FALSE)
create_report("analysis", template_name = "temple")
} # }
```
