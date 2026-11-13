# Install a Package, Bypassing an Existing Lock

Calls
[`install.packages`](https://rdrr.io/r/utils/install.packages.html) with
`INSTALL_opts = "--no-lock"`, for the case where an interrupted
installation left a `00LOCK` directory behind. The packages come from
the repositories given in `repos` (by default the session's
`getOption("repos")`); nothing else about the source is checked, so
install only packages you would install with
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Usage

``` r
install_package_no_lock(packages, repos = getOption("repos"))
```

## Arguments

- packages:

  Character vector of package name(s) to install. Each must be a valid
  package name (letters, digits and dots, starting with a letter): a
  path, a URL or an option string is refused.

- repos:

  Repositories to install from, passed to
  [`install.packages`](https://rdrr.io/r/utils/install.packages.html).

## Value

Invisibly, `NULL` (as per
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
install_package_no_lock("dplyr")
} # }
```
