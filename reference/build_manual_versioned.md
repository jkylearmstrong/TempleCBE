# Build a Versioned PDF Reference Manual

Renders the package's Rd documentation to a PDF manual (via
`R CMD Rd2pdf`) named with the current package version, so successive
builds never collide or overwrite each other. Writes into
`pkgdown/assets/manual/` by default: pkgdown copies `pkgdown/assets/*`
verbatim into `docs/` on every
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html),
which is what makes a navbar link to the manual resolve on the published
site. (`inst/manual/` would not be copied there, and would also ship
with every installed copy of the package.)

## Usage

``` r
build_manual_versioned(pkg = ".", path = NULL, latest = TRUE)
```

## Arguments

- pkg:

  Path to the package, passed to
  [`as.package`](https://devtools.r-lib.org/reference/as.package.html).

- path:

  Directory to write the PDF into. Defaults to `pkgdown/assets/manual/`
  under the package root.

- latest:

  Logical (default `TRUE`); if `TRUE`, also writes a stable
  `<package>_latest.pdf` copy alongside the versioned file, so a
  permanent link (e.g. in a pkgdown navbar) doesn't need to change on
  every version bump.

## Value

The path to the versioned PDF, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
build_manual_versioned()
} # }
```
