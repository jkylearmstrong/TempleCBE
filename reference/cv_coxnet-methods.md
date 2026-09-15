# Use a \`cv_coxnet\` Result

Predict from, tidy, plot, or collect the metrics of a \[cv_coxnet()\]
result. Predictions and coefficients come from the model refit to all
the data at the chosen \`mixture\`.

## Usage

``` r
# S3 method for class 'cv_coxnet'
predict(
  object,
  new_data,
  type = c("linear_pred", "survival"),
  penalty = "lambda.min",
  eval_time = NULL,
  increasing = TRUE,
  ...
)

# S3 method for class 'cv_coxnet'
tidy(x, penalty = "lambda.min", ...)

# S3 method for class 'cv_coxnet'
collect_metrics(x, ..., summarize = TRUE)

# S3 method for class 'cv_coxnet'
autoplot(object, ...)
```

## Arguments

- object, x:

  A \[cv_coxnet()\] result.

- new_data:

  A data frame of new predictors.

- type, eval_time, increasing:

  As for \[predict.coxnet_model()\].

- penalty:

  \`"lambda.min"\` (default), \`"lambda.1se"\`, or a number.

- ...:

  Not used.

- summarize:

  For \`collect_metrics()\`: \`TRUE\` for means over resamples,
  \`FALSE\` for each resample's metrics.

## Value

\`predict()\`: a tibble as from \[predict.coxnet_model()\]. \`tidy()\`:
the coefficients at \`penalty\`. \`collect_metrics()\`: a tibble of
metrics. \`autoplot()\`: a ggplot of the selection metric against the
penalty, with \`lambda.min\` (solid) and \`lambda.1se\` (dashed) marked.
