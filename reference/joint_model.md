# Joint Survival-Status-Time Model

Fits a coordinated trio of predictive models on clinical survival data:

1.  **Survival Model (`coxnet`)**: Penalized Cox proportional hazards
    model on `Surv(time, status)` or start/stop counting-process data
    `Surv(tstart, tstop, status)` via
    [`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
    and
    [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md),
    correctly handling right-censoring and time-varying intervals.

2.  **Status Model (`status`)**: Binary event classification on
    `status ~ x` via penalized logistic regression (glmnet) or bagged
    trees (baguette), with optional probability calibration via
    probably.

3.  **Time Model (`time`)**: Continuous duration regression on
    `time ~ x` via penalized linear regression (glmnet) or bagged trees
    (baguette).

## Usage

``` r
joint_model(
  data,
  outcome = survival::Surv(time, status) ~ .,
  subject_id = NULL,
  engine = c("glmnet", "baguette", "stacks"),
  calibration = TRUE,
  mixture = 1,
  penalty = NULL,
  eval_time = NULL,
  covariates = c("path", "baseline"),
  ...
)
```

## Arguments

- data:

  A data frame containing the survival outcome and predictors.

- outcome:

  A formula containing a
  [`Surv`](https://rdrr.io/pkg/survival/man/Surv.html) outcome, such as
  `Surv(time, status) ~ .` or `Surv(tstart, tstop, status) ~ .`.

- subject_id:

  Optional character string specifying the subject identifier column for
  counting-process (start/stop) data.

- engine:

  Character string specifying the modeling engine: `"glmnet"` (default),
  `"baguette"` (bagged decision trees), or `"stacks"` (the `"baguette"`
  models plus a Cox meta-learner whose predictions
  [`predict()`](https://rdrr.io/r/stats/predict.html) adds as extra
  columns; see Details).

- calibration:

  Logical; whether to fit a probability calibration model on the status
  predictions using probably (default `TRUE`), fitted on out-of-fold
  predictions; see Details.

- mixture:

  Elastic net mixing parameter for glmnet models (default `1` for
  lasso).

- penalty:

  Penalty value for the `coxnet` survival model. If `NULL` (default),
  tuned automatically via internal cross-validation using the Integrated
  Brier Score. The glmnet status and time models always choose their own
  penalty (`lambda.min`).

- eval_time:

  Optional vector of evaluation times for dynamic survival
  probabilities. Defaults to deciles of uncensored event times.

- covariates:

  `"path"` (the default) or `"baseline"`; as in
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md).
  Used only when `penalty` is `NULL`, to score the internal
  cross-validation that tunes the penalty of the `coxnet` model on
  start/stop data.

- ...:

  Additional arguments passed to
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  (when `penalty` is `NULL`) or
  [`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md),
  and so to
  [`glmnet`](https://glmnet.stanford.edu/reference/glmnet.html) for the
  survival model only.

## Value

An S3 object of class `c("joint_model", "hardhat_model")` with elements:

- coxnet_model:

  Fitted penalized Cox proportional hazards model.

- status_model:

  Fitted binary event classification model.

- time_model:

  Fitted continuous follow-up duration model.

- stack_model:

  Fitted Cox meta-learner (if `engine = "stacks"`);
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns its output
  as `.pred_stack_linear_pred` and `.pred_stack_risk_score`.

- calibration_model:

  Probability calibration model from probably (`NULL` if
  `calibration = FALSE` or if it was skipped with a warning).

- components:

  List of extracted outcome variables, formulas, the hardhat blueprint,
  and the raw (pre-blueprint) predictor columns, `raw_predictors`.

- engine:

  Selected modeling engine.

- eval_time:

  Evaluation horizons used for survival scoring.

## Details

With `engine = "stacks"` the status and time models are the bagged trees
of `engine = "baguette"`, and a penalized Cox meta-learner (glmnet) is
also fit on the standardized predictions of the three models and stored
as `stack_model`.
[`predict.joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/predict.joint_model.md)
returns the meta-learner's linear predictor and risk score as the extra
columns `.pred_stack_linear_pred` and `.pred_stack_risk_score`; the
survival, status and time predictions are exactly those of `"baguette"`,
and the stacks package is not involved. A warning says so.

This joint framework enables clinical researchers to investigate and
contrast true survival modeling against naive classification and
regression proxies as discussed in clinical literature (e.g., Rizopoulos
2015, PMC4503792).

Predictors are processed through the hardhat
[`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
formula blueprint (one-hot dummy encoding, no intercept), the same
preprocessing used by
[`coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
and
[`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md),
so the status and time sub-models see exactly the columns the survival
sub-model does, and \`predict()\` re-applies the training encoding via
[`forge`](https://hardhat.tidymodels.org/reference/forge.html) rather
than recomputing dummy columns from scratch.

The status calibrator is fitted on out-of-fold predictions, never on the
training rows' own predictions, which are overfit (a bagged tree
classifies its training rows almost perfectly, and a calibrator fitted
on them amplifies the overfit). For `engine = "glmnet"` these are
[`cv.glmnet`](https://glmnet.stanford.edu/reference/cv.glmnet.html)'s
prevalidated predictions at `lambda.min`; for the bagged trees, the
predictions of an inner 5-fold cross-validation. With start/stop data
and a `subject_id`, the folds are grouped by subject, so a subject's
intervals are never split between the model and the predictions used to
calibrate it; the same folds select the penalty of the glmnet status and
time models. When out-of-fold predictions cannot be produced, a warning
says so and the status probabilities are returned uncalibrated.

## See also

[`cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_joint_model.md),
[`nested_cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/nested_cv_joint_model.md),
[`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md),
[`glmnet_IBS`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE)) {
  set.seed(42)
  df <- data.frame(
    time = stats::rexp(60, rate = 0.05),
    status = stats::rbinom(60, 1, 0.6),
    x1 = stats::rnorm(60),
    x2 = stats::rnorm(60)
  )
  fit <- joint_model(df, survival::Surv(time, status) ~ x1 + x2)
  print(fit)
  preds <- predict(fit, new_data = df[1:5, ])
}
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics
#> === TempleCBE Joint Survival-Status-Time Model ===
#> Engine: glmnet | Calibration: TRUE
#> Outcome: survival::Surv(time, status) (Type: right)
#> Sample Size: 60 observations | Predictors: 2
#> Events: 28 (46.7%) | Median Follow-up Time: 13.17
#> Fitted Sub-Models:
#>   1. Cox Survival Model: cv_coxnet
#>   2. Status Classification Model: cv.glmnet
#>   3. Time Duration Model: cv.glmnet
# }
```
