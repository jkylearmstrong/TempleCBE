# Plot Penalty Lambda Versus Integrated Brier Score

Visualizes \`log10(lambda)\` against Integrated Brier Score across
resamples, colored by \`alpha\`.

## Usage

``` r
plot_tune_lambda_ibs(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[summarize_tune_results()\].

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.
