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

  Qualitative: cherry, dark blue, ochre, taupe, black, lime, geranium.
  At most 7 colors.

- `"diverging"`:

  Dark blue through white to cherry, for values centered at zero
  (correlations, loadings, differences).

- `"sequential"`:

  Eggshell to cherry.

## Examples

``` r
temple_pal()(3)
#> [1] "#a41e35" "#21287e" "#fdb913"
temple_pal("diverging")(5)
#> [1] "#21287E" "#9093BE" "#FFFFFF" "#D18E9A" "#A41E35"
```
