# Plot Penalty Lambda Versus Integrated Brier Score

Visualizes \`log10(lambda)\` against Integrated Brier Score across
resamples, colored by \`alpha\`.

## Usage

``` r
plot_tune_lambda_ibs(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[tune_alpha_by_split()\].

- label_map:

  Optional label mapping (see \[cbe_labels()\]). The titles of the axes
  and of the legend are looked up under the terms \`"lambda"\`,
  \`"IBS"\` and \`"alpha"\`; a term that is not in the map keeps its
  default title.

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.
