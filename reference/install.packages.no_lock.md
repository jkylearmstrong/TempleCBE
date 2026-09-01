# Install a Package, Bypassing an Existing Lock

Install a Package, Bypassing an Existing Lock

## Usage

``` r
install.packages.no_lock(packages)
```

## Arguments

- packages:

  Character vector of package name(s) to install.

## Value

Invisibly, `NULL` (as per
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
install.packages.no_lock("dplyr")
} # }
```
