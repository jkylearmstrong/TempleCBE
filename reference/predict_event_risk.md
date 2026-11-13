# Predicted Event Risk at One Horizon from a Fitted Survival Model

Predicts the event probability by \`horizon\` (one minus the survival
probability) for new data under a fitted \[coxnet()\], \[cv_coxnet()\],
\[joint_model()\], \[cox_time_varying()\] or \[fine_gray()\] model. This
is the model-level adapter: for the raw \`predict()\` output, use
\[extract_horizon_risk()\]. For a \[fine_gray()\] fit the risk is the
cumulative incidence (CIF) of the cause of interest at \`horizon\`, not
one minus a survival probability.

## Usage

``` r
predict_event_risk(object, new_data, horizon, penalty = NULL, ...)
```

## Arguments

- object:

  A fitted \`coxnet_model\`, \`cv_coxnet\`, \`joint_model\`,
  \`cox_time_varying\` or \`fine_gray_model\` object.

- new_data:

  A data frame of new predictors, as for \`predict()\`.

- horizon:

  A single finite, positive time: the horizon the risk applies to.

- penalty:

  For \`coxnet_model\`: the penalty to predict at; defaults to the
  model's \`penalty\`. For \`cv_coxnet\`: \`"lambda.min"\` (default),
  \`"lambda.1se"\`, or a number. Ignored for \`joint_model\`,
  \`cox_time_varying\` and \`fine_gray_model\`.

- ...:

  Not used.

## Value

A numeric vector of event probabilities by \`horizon\`, one per row of
\`new_data\`.

## Details

(A plain function dispatching on the model class, not an S3 generic, so
it works as written: the S3 split is a follow-up for the roxygen run
that regenerates \`NAMESPACE\`.)

## See also

\[extract_horizon_risk()\], \[calibration_from_model()\],
\[decision_curve_from_model()\]

## Examples

``` r
set.seed(1)
n <- 120
d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
                age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
predict_event_risk(fit, d[1:5, ], horizon = 10)
#> [1] 0.215163 0.215163 0.215163 0.215163 0.215163
```
