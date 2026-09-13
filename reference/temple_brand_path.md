# Path to the Bundled Temple brand.yml

A copy of the quarto_temple_brand extension's `brand.yml`, for tools
that read brand files directly, such as
[`quarto::theme_brand_ggplot2()`](https://quarto-dev.github.io/quarto-r/reference/theme_helpers.html)
or `bslib::bs_theme(brand = )`.

## Usage

``` r
temple_brand_path()
```

## Value

Path to `brand.yml` inside the installed package.

## Examples

``` r
temple_brand_path()
#> [1] "/home/runner/.cache/R/renv/library/TempleCBE-357df843/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu/TempleCBE/brand/brand.yml"
```
