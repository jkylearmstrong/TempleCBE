# Predict Method for Joint Models

Generates multi-paradigm predictions from a fitted
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md):
dynamic survival probabilities from the Cox model, binary event
probabilities from the status model (both raw and calibrated), and
expected duration from the time model.

## Usage

``` r
# S3 method for class 'joint_model'
predict(object, new_data = NULL, eval_time = NULL, ...)
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

- ...:

  Additional arguments passed to underlying predict methods.

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
