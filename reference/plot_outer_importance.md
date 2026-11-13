# Plot Outer-Fold Coefficient Importance and Selection Frequency

Visualizes predictor coefficient distributions and selection frequencies
across outer resampling folds. Accepts output from
\[tidy_outer_coefficients()\], \[score_outer()\], or
\[summarize_outer_importance()\].

## Usage

``` r
plot_outer_importance(
  x,
  top_n = NULL,
  nonzero_only = FALSE,
  type = c("boxplot", "frequency"),
  label_map = NULL,
  style = NULL
)
```

## Arguments

- x:

  A tibble from \[tidy_outer_coefficients()\], \[score_outer()\], or
  \[summarize_outer_importance()\].

- top_n:

  Optional integer restricting the plot to the top \`top_n\` predictors
  ranked by selection frequency and absolute median coefficient.

- nonzero_only:

  Logical (default \`FALSE\`). When \`TRUE\`, drops zero coefficient
  estimates before drawing boxplots.

- type:

  Plot type: \`"boxplot"\` (default; horizontal boxplot or range
  intervals of coefficients annotated with selection percentage) or
  \`"frequency"\` (bar/lollipop chart of selection frequency).

- label_map:

  Optional label mapping passed to \[resolve_cbe_labels()\].

- style:

  Label style (\`"short"\` or \`"full"\`) when resolving \`label_map\`.

## Value

A \`ggplot\` object.

## See also

\[summarize_outer_importance()\], \[tidy_outer_coefficients()\],
\[score_outer()\]
