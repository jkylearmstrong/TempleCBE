# Plot Integrated Brier Score Against Alpha With Dynamic Model Fit

Visualizes the relationship between elastic net mixing parameter
\`alpha\` and Integrated Brier Score across resampling folds, overlaying
the fitted line from \[fit_ibs_mixed_model()\] and its dynamic equation.

## Usage

``` r
plot_tune_alpha_ibs(data, log_ibs = TRUE, show_points = TRUE, show_line = TRUE)
```

## Arguments

- data:

  A data frame from \[summarize_tune_results()\].

- log_ibs:

  Logical. When \`TRUE\` (default), plots \`log10(IBS)\`.

- show_points:

  Logical. Plot individual resample points. Default \`TRUE\`.

- show_line:

  Logical. Overlay the fitted regression line. Default \`TRUE\`.

## Value

A \`ggplot2\` object.

## See also

\[fit_ibs_mixed_model()\], \[plot_tune_lambda_ibs()\],
\[plot_tune_coef_boxplot()\]
