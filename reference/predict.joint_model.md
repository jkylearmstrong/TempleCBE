# Predict Method for Joint Models

Generates multi-paradigm predictions from a fitted
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md):
dynamic survival probabilities from the Cox model, binary event
probabilities from the status model (both raw and calibrated), and
expected duration from the time model.

## Usage

``` r
# S3 method for class 'joint_model'
predict(object, new_data = NULL, eval_time = NULL, type = NULL, ...)
```

## Arguments

- object:

  A `joint_model` object.

- new_data:

  Optional new data frame to predict upon. If `NULL`, predicts on
  training data.

- eval_time:

  Horizon times for survival probabilities. Defaults to the model's
  `eval_time`.

- type:

  Optional single prediction type, to return only that column as a
  one-column tibble: `"survival"`, `"time"` (or `"numeric"`), `"prob"`
  (calibrated status probability), `"status"`, `"linear_pred"`,
  `"risk_score"`, `"stack_linear_pred"` or `"stack_risk_score"`. `NULL`
  (the default) or `"all"` returns every column.

- ...:

  Not used; checked with
  [`check_dots_empty`](https://rlang.r-lib.org/reference/check_dots_empty.html).

## Value

A tibble with columns:

- .pred_survival:

  Nested list of survival probability curves over `eval_time`.

- .pred_status:

  Predicted event probability from the status model.

- .pred_status_calibrated:

  Calibrated event probability (if calibration was enabled).

- .pred_time:

  Predicted duration from the time model.

- .pred_linear_pred:

  Linear predictor from the Cox model (higher = longer survival).

- .pred_risk_score:

  Relative hazard risk score from the Cox model.

- .pred_stack_linear_pred:

  Linear predictor from the stacked meta-learner (if
  `engine = "stacks"`).

- .pred_stack_risk_score:

  Relative hazard risk score from the stacked meta-learner (if
  `engine = "stacks"`).

## Details

Predictors are re-derived from `new_data` with
[`forge`](https://hardhat.tidymodels.org/reference/forge.html) against
the model's training blueprint, so factor levels and dummy columns match
training exactly, even if `new_data` does not exhibit every level.

The result has one row per row of `new_data`, also for start/stop data,
and each row is predicted from its own covariates, taken as constant
from time 0. That is not how a subject's survival is scored along their
start/stop covariate path; see
[`cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_joint_model.md)
(argument `covariates`).
