# Temple Color Palettes

Returns a palette function that generates `n` Temple brand colors.

## Usage

``` r
temple_pal(palette = c("main", "diverging", "sequential"), reverse = FALSE)
```

## Arguments

- palette:

  One of `"main"`, `"diverging"`, or `"sequential"`.

- reverse:

  Logical; reverse the color order.

## Value

A function taking `n` and returning `n` hex codes.

## Details

- `"main"`:

  Qualitative: cherry, Night Owl, Owl's Eye, Founder's Garden, Upward
  Momentum, Diamond Acres, black. At most 7 colors.

- `"diverging"`:

  Night Owl through white to cherry, for values centered at zero
  (correlations, loadings, differences).

- `"sequential"`:

  Book Nook to cherry.

## Examples

``` r
temple_pal()(3)
#> [1] "#a41e35" "#005a70" "#f3aa00"
temple_pal("diverging")(5)
#> [1] "#005A70" "#7FACB7" "#FFFFFF" "#D18E9A" "#A41E35"
```
