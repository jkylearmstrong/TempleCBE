# Plot Coefficient Estimates Distribution Across Tuning Resamples

Generates horizontal boxplots showing the stability and shrinkage of
coefficient estimates for each feature across resamples.

## Usage

``` r
plot_tune_coef_boxplot(data, top_n = NULL, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[summarize_tune_results()\].

- top_n:

  Optional integer limit on the number of features to plot (ordered by
  median absolute effect size).

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.
