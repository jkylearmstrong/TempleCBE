# Plot a \`cbe_nested_survival_run\` Object

Plot a \`cbe_nested_survival_run\` Object

## Usage

``` r
# S3 method for class 'cbe_nested_survival_run'
plot(
  x,
  type = c("importance", "frequency", "outer_ibs", "tuning", "censoring"),
  top_n = NULL,
  ...
)

# S3 method for class 'cbe_nested_survival_run'
autoplot(
  object,
  type = c("importance", "frequency", "outer_ibs", "tuning", "censoring"),
  top_n = NULL,
  ...
)
```

## Arguments

- x, object:

  A \[nested_survival_run()\] object.

- type:

  Plot type:

  \`"importance"\`

  :   (Default) Outer-fold coefficient importance boxplot via
      \[plot_outer_importance()\].

  \`"frequency"\`

  :   Outer-fold predictor selection frequency bar chart via
      \[plot_outer_importance()\].

  \`"outer_ibs"\`

  :   Distribution of outer-fold Integrated Brier Scores across
      candidate models.

  \`"tuning"\`

  :   Inner tuning Integrated Brier Score across \`alpha\`.

  \`"censoring"\`

  :   Scatter comparison of unweighted vs IPCW outer-fold IBS.

- top_n:

  Optional integer restricting importance plots to the top \`top_n\`
  predictors.

- ...:

  Additional arguments passed to \[plot_outer_importance()\].

## Value

A \`ggplot\` object.
