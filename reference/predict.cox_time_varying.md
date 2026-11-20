# Predict From a \`cox_time_varying\` Model

Predict From a \`cox_time_varying\` Model

## Usage

``` r
# S3 method for class 'cox_time_varying'
predict(
  object,
  new_data,
  type = c("linear_pred", "survival"),
  eval_time = NULL,
  ...
)
```

## Arguments

- object:

  A \[cox_time_varying()\] fit.

- new_data:

  A data frame of new predictors.

- type:

  \`"linear_pred"\` for the linear predictor at \`eval_time\` (larger
  means higher risk), or \`"survival"\` for survival probabilities at
  \`eval_time\`.

- eval_time:

  For \`type = "linear_pred"\`, the single time the linear predictor is
  evaluated at (required: it varies with time). For \`type =
  "survival"\`, the times to predict survival at (required).

- ...:

  Not used.

## Value

A tibble with one row per row of \`new_data\`: \`.pred_linear_pred\`, or
\`.pred\`, a list-column of tibbles with \`.eval_time\` and
\`.pred_survival\`.

Survival is \\S(t \mid x) = \exp(-\sum\_{t_j \le t} \Delta H_0(t_j)
e^{lp(t_j)})\\, with \\\Delta H_0\\ the Breslow increments of
\[tvc_breslow()\] and \\lp(t_j)\\ the linear predictor at the event time
\`t_j\`; the sum runs over the fitted event times, so survival is flat
past the last one.
