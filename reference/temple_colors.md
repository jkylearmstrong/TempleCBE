# Temple University Brand Colors

Hex codes for the Temple University palette used by the
[quarto_temple_brand](https://github.com/jkylearmstrong/quarto_temple_brand)
Quarto extension, so R graphics match branded reports.

## Usage

``` r
temple_colors(...)
```

## Arguments

- ...:

  Optional color names (e.g. `"cherry"`, `"dark-blue"`). With none,
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
#>    cherry     black     white     taupe  icy-blue      lime  eggshell     ochre 
#> "#a41e35" "#1d1d1d" "#ffffff" "#baa682" "#cbf6ff" "#c7d703" "#f2eee8" "#fdb913" 
#>  geranium dark-blue 
#> "#e7201d" "#21287e" 
temple_colors("cherry", "dark-blue")
#>    cherry dark-blue 
#> "#a41e35" "#21287e" 
```
