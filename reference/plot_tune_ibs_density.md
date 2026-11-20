# Density of the Integrated Brier Score per Resample

Density curves of the IBS over the tuning grid, one per inner resample,
either overlaid or in facets. The box-plot view of the same data is
\[plot_tune_ibs_by_resample()\].

## Usage

``` r
plot_tune_ibs_density(
  data,
  facet = FALSE,
  log_ibs = FALSE,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- data:

  A tuning table with \`IBS\` and a resample column
  (\`inner_resamples_splits\`, \`inner_resamples\` or \`split\`).
  Without a resample column one overall density is drawn.

- facet:

  If \`TRUE\`, one panel per resample; default \`FALSE\` overlays the
  densities.

- log_ibs:

  If \`TRUE\`, the x-axis is \`log10(IBS)\`. Default \`FALSE\`.

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.

## See also

\[plot_tune_ibs_by_resample()\], \[tbl_tune_alpha()\]

## Examples

``` r
set.seed(1)
tune <- data.frame(split = rep(paste0("Fold", 1:3), each = 20), alpha = 0.5,
                   lambda = rep(seq(0.01, 0.2, length.out = 20), 3))
tune$IBS <- 0.15 + tune$lambda / 5 + stats::rnorm(60, 0, 0.01)
plot_tune_ibs_density(tune)
```
