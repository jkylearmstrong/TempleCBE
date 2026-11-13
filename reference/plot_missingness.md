# Missing Data Audit Plot

Generates dodged percentage bar charts comparing data completeness
across modalities.

## Usage

``` r
plot_missingness(data, base_size = 13)
```

## Arguments

- data:

  Data frame with columns: `parameter`, `pct` (fraction missing),
  `method`.

- base_size:

  Base font size (default: 13).

## Value

A ggplot2 object.
