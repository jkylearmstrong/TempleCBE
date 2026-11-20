# Calibration Table of a Fitted Survival Model

Validates a fitted \[coxnet()\], \[cv_coxnet()\], \[joint_model()\],
\[cox_time_varying()\] or \[fine_gray()\] model at one horizon: the
event risk is predicted with \[predict_event_risk()\] and passed, with
the right-censored outcome from \`new_data\`, to
\[calibration_table()\]. See there for the bins, the observed risks and
the metrics; competing events are treated as censoring, as in
\[calibration_table()\]. For a \[fine_gray()\] fit the risk is the
cumulative incidence of the cause of interest and the outcome is
dichotomised to that cause (\`event_col == failcode\`); other causes
count as censoring.

## Usage

``` r
calibration_from_model(
  object,
  new_data,
  time_col,
  event_col,
  horizon,
  penalty = NULL,
  ...
)
```

## Arguments

- object:

  A fitted \`coxnet_model\`, \`cv_coxnet\`, \`joint_model\`,
  \`cox_time_varying\` or \`fine_gray_model\` object.

- new_data:

  A data frame with the predictors and the \`time_col\` / \`event_col\`
  outcome columns. For a \`fine_gray_model\` fit, \`event_col\` holds
  the competing-risk event codes (censoring code, then one code per
  cause), as in \[fine_gray()\].

- time_col, event_col:

  Outcome columns in \`new_data\`: follow-up time and the 0/1 event
  indicator (or, for a \`fine_gray_model\` fit, the event codes).

- horizon:

  A single finite, positive time: the horizon the calibration applies
  to.

- penalty:

  Passed to \[predict_event_risk()\]; ignored for \`joint_model\`,
  \`cox_time_varying\` and \`fine_gray_model\`.

- ...:

  Passed to \[calibration_table()\] (for example \`bins\` or
  \`conf_level\`).

## Value

As \[calibration_table()\]: a tibble with one row per model and bin,
with \`attr(x, "metrics")\` and \`attr(x, "horizon")\`. For Surv
outcomes the metrics are \`NA\` (see Details).

## Details

\*\*No summary metrics for Surv outcomes.\*\* \`attr(x, "metrics")\` is
always \`NA\` for \`intercept\`, \`slope\` and \`brier\` here:
\[calibration_table()\] computes those only for a binary outcome, and no
IPCW-based summary metrics for right-censored outcomes are implemented.
The bins table itself (mean predicted vs Kaplan-Meier observed risk per
bin) is the calibration assessment. To estimate a calibration slope
yourself, dichotomise the outcome at the horizon — 1 for an event by
\`horizon\`, 0 for follow-up past \`horizon\` (subjects censored before
\`horizon\` have unknown status and are dropped) — and regress that
indicator on the logit of the predicted risk, for example
\`glm(y_horizon ~ qlogis(risk), family = binomial())\`: the coefficient
of \`qlogis(risk)\` is the slope, and the intercept of \`glm(y_horizon ~
1 + offset(qlogis(risk)), family = binomial())\` is
calibration-in-the-large.

## See also

\[decision_curve_from_model()\], \[predict_event_risk()\],
\[calibration_table()\]

## Examples

``` r
set.seed(1)
n <- 120
d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
                age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
#> TempleCBE: Pinned Cox tie-handling to 'breslow' (option 'templecbe.cox_ties').
calibration_from_model(fit, d, "time", "status", horizon = 10, bins = 5)
#> # A tibble: 1 × 7
#>   model   bin     n mean_pred observed conf_low conf_high
#>   <chr> <int> <int>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 model     1   120     0.215    0.216    0.150     0.305
```
