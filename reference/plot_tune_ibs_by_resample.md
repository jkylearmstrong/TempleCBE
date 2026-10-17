# Plot Integrated Brier Score Distribution Across Resampling Splits

Displays the distribution of IBS across cross-validation or bootstrap
splits.

## Usage

``` r
plot_tune_ibs_by_resample(data, label_map = NULL, style = NULL)
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
