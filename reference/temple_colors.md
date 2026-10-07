# Temple University Brand Colors

Hex codes for the Temple University palette used by the
[quarto_temple_brand](https://github.com/jkylearmstrong-temple/quarto_temple_brand)
Quarto extension, so R graphics match branded reports. The palette
follows Temple's current brand
(<https://liberalarts.temple.edu/marcom/logos-and-brand>): primary
cherry, white, and black; secondary Clear Skies and Book Nook; formal
accents Academic Gold, Diamond Acres, Founder's Garden, and Night Owl;
and casual accents Owl's Eye, Conwell Blue, Upward Momentum, and Cherry
Blossom.

## Usage

``` r
temple_colors(...)
```

## Arguments

- ...:

  Optional color names (e.g. `"cherry"`, `"night-owl"`). With none,
  every color is returned.

## Value

A named character vector of hex codes.

## See also

[`temple_pal`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_pal.md),
[`scale_colour_temple`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md),
[`theme_temple`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_temple.md)

## Examples

``` r
temple_colors()
#>          cherry           white           black     clear-skies       book-nook 
#>       "#a41e35"       "#ffffff"       "#000000"       "#deefec"       "#fff2e8" 
#>   academic-gold   diamond-acres founders-garden       night-owl        owls-eye 
#>       "#ad7422"       "#9e9597"       "#772762"       "#005a70"       "#f3aa00" 
#>    conwell-blue upward-momentum  cherry-blossom 
#>       "#12d0ff"       "#1fceb6"       "#fe649f" 
temple_colors("cherry", "night-owl")
#>    cherry night-owl 
#> "#a41e35" "#005a70" 
```
