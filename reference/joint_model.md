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

4.  **Stack Ensemble (`stack`)**: Optional regularized meta-learner
    blending candidate predictions via stacks.

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
  `"baguette"` (bagged decision trees), or `"stacks"` (stacked
  ensemble).

- calibration:

  Logical; whether to fit a probability calibration model on the status
  predictions using probably (default `TRUE`).

- mixture:

  Elastic net mixing parameter for glmnet models (default `1` for
  lasso).

- penalty:

  Penalty value for glmnet. If `NULL` (default), tuned automatically via
  internal cross-validation using the Integrated Brier Score.

- eval_time:

  Optional vector of evaluation times for dynamic survival
  probabilities. Defaults to deciles of uncensored event times.

- ...:

  Additional arguments passed to
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  or [`glmnet`](https://glmnet.stanford.edu/reference/glmnet.html).

## Value

An S3 object of class `c("joint_model", "list")` with elements:

- coxnet_model:

  Fitted penalized Cox proportional hazards model.

- status_model:

  Fitted binary event classification model.

- time_model:

  Fitted continuous follow-up duration model.

- stack_model:

  Fitted stacks ensemble (if `engine = "stacks"`).

- calibration_model:

  Probability calibration model from probably (if `calibration = TRUE`).

- components:

  List of extracted outcome variables, formulas, and baseline hazard.

- engine:

  Selected modeling engine.

- eval_time:

  Evaluation horizons used for survival scoring.

## Details

This joint framework enables clinical researchers to investigate and
contrast true survival modeling against naive classification and
regression proxies as discussed in clinical literature (e.g., Rizopoulos
2015, PMC4503792).

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
