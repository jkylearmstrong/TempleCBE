# Predict From a \`joint_readmission_death\` Model

Predict From a \`joint_readmission_death\` Model

## Usage

``` r
# S3 method for class 'joint_readmission_death'
predict(
  object,
  new_data,
  type = c("death_survival", "readmission_cumhaz"),
  eval_time = NULL,
  n_prior_readmissions = 0,
  ...
)
```

## Arguments

- object:

  A \[joint_readmission_death()\] fit.

- new_data:

  A data frame of new predictors (one row per subject, with the
  predictors of both sub-models).

- type:

  \`"death_survival"\` for survival from death at \`eval_time\`, or
  \`"readmission_cumhaz"\` for the expected cumulative number of
  readmissions by \`eval_time\`.

- eval_time:

  Times to predict at (required).

- n_prior_readmissions:

  Number of prior readmissions the death-survival prediction is
  conditional on (default \`0\`, i.e. no readmission yet). The
  prediction does not integrate over the readmission process; it is the
  death survival of a subject with that many prior readmissions
  throughout.

- ...:

  Not used.

## Value

A tibble with one row per row of \`new_data\`: \`.pred\`, a list-column
of tibbles with \`.eval_time\` and \`.pred_survival\` (death survival)
or \`.pred_cum_readmissions\` (expected cumulative readmissions).

Death survival is \\S(t) = \exp(-H_0(t) e^{lp})\\, with \\H_0\\ the
Breslow baseline hazard of the death sub-model and \`lp\` the linear
predictor at \`n_prior_readmissions\`. Expected cumulative readmissions
are \\H_0(t) e^{lp}\\ from the readmission sub-model's Breslow baseline
hazard. Predictions use a frailty of 1 (the population mean) when the
fit used a frailty. In this draft the predictors of both sub-models must
be numeric columns of \`new_data\`.
