# Temple Color and Fill Scales for ggplot2

Temple Color and Fill Scales for ggplot2

## Usage

``` r
scale_colour_temple(
  palette = c("main", "diverging", "sequential"),
  discrete = NULL,
  reverse = FALSE,
  midpoint = 0,
  ...
)

scale_color_temple(
  palette = c("main", "diverging", "sequential"),
  discrete = NULL,
  reverse = FALSE,
  midpoint = 0,
  ...
)

scale_fill_temple(
  palette = c("main", "diverging", "sequential"),
  discrete = NULL,
  reverse = FALSE,
  midpoint = 0,
  ...
)
```

## Arguments

- palette:

  One of `"main"`, `"diverging"`, or `"sequential"`.

- discrete:

  Logical; a discrete scale (default for `"main"`) or a continuous
  gradient.

- reverse:

  Logical; reverse the color order.

- midpoint:

  For continuous `"diverging"` scales, the data value mapped to white
  (default 0).

- ...:

  Passed to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
  [`scale_colour_gradient2`](https://ggplot2.tidyverse.org/reference/scale_gradient.html),
  or
  [`scale_colour_gradientn`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A ggplot2 scale.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  scale_colour_temple()


ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  scale_fill_temple("sequential", discrete = FALSE)
```
