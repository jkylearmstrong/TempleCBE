# Predict From a \`coxnet\` Model

Predict From a \`coxnet\` Model

## Usage

``` r
# S3 method for class 'coxnet_model'
predict(
  object,
  new_data,
  type = c("linear_pred", "survival"),
  penalty = NULL,
  eval_time = NULL,
  increasing = TRUE,
  ...
)
```

## Arguments

- object:

  A \[coxnet()\] model.

- new_data:

  A data frame (or matrix) of new predictors.

- type:

  \`"linear_pred"\` for the linear predictor, or \`"survival"\` for
  survival probabilities at \`eval_time\`.

- penalty:

  The penalty to predict at; defaults to the model's \`penalty\`.

- eval_time:

  For \`type = "survival"\`, the times to predict survival at.

- increasing:

  For \`type = "linear_pred"\`: if \`TRUE\` (the default, as in
  tidymodels' censored package), the sign is flipped so larger values
  mean longer survival. Use \`FALSE\` for glmnet's own sign, where
  larger values mean higher risk.

- ...:

  Not used.

## Value

A tibble with one row per row of \`new_data\`: \`.pred_linear_pred\`, or
\`.pred\`, a list-column of tibbles with \`.eval_time\` and
\`.pred_survival\`.

Survival is \\S(t \mid x) = \exp(-H_0(t) e^{x^\top \beta})\\, with
\\H_0\\ the Breslow estimate of the cumulative baseline hazard from the
training data at \`penalty\`. Each row's covariates are taken as
constant from time 0; to predict along a subject's start/stop covariate
path, and score it, use \[cv_coxnet()\].
