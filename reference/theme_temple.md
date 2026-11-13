# Temple ggplot2 Theme

A minimal theme with Temple cherry titles and facet strips, matching
reports rendered with the quarto_temple_brand extension.

## Usage

``` r
theme_temple(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size in points.

- base_family:

  Base font family. The brand's body typeface is `"Faustina"` (headings
  use `"Roboto"`); the default `""` uses the device font, because naming
  a font that isn't installed makes devices warn.

## Value

A ggplot2 theme.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(colour = temple_colors("cherry")) +
  facet_wrap(~cyl) +
  labs(title = "Weight vs. mileage") +
  theme_temple()
```
