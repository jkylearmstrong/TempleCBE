# Decision Curve of a Fitted Survival Model

Validates a fitted \[coxnet()\], \[cv_coxnet()\], \[joint_model()\],
\[cox_time_varying()\] or \[fine_gray()\] model at one horizon: the
event risk is predicted with \[predict_event_risk()\] and passed, with
the right-censored outcome from \`new_data\`, to \[decision_curve()\].
See there for the net-benefit definition; competing events are treated
as censoring, as in \[decision_curve()\]. For a \[fine_gray()\] fit the
risk is the cumulative incidence of the cause of interest and the
outcome is dichotomised to that cause (\`event_col == failcode\`); other
causes count as censoring.

## Usage

``` r
decision_curve_from_model(
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
  the competing-risk event codes, as in \[fine_gray()\].

- time_col, event_col:

  Outcome columns in \`new_data\`: follow-up time and the 0/1 event
  indicator (or, for a \`fine_gray_model\` fit, the event codes).

- horizon:

  A single finite, positive time: the horizon the decision curve applies
  to.

- penalty:

  Passed to \[predict_event_risk()\]; ignored for \`joint_model\`,
  \`cox_time_varying\` and \`fine_gray_model\`.

- ...:

  Passed to \[decision_curve()\] (for example \`thresholds\`).

## Value

As \[decision_curve()\]: a tibble of class \`decision_curve\` with one
row per threshold.

## See also

\[calibration_from_model()\], \[predict_event_risk()\],
\[decision_curve()\]

## Examples

``` r
set.seed(1)
n <- 120
d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
                age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
head(decision_curve_from_model(fit, d, "time", "status", horizon = 10))
#> # A tibble: 6 × 5
#>   model threshold net_benefit treat_all prop_above
#>   <chr>     <dbl>       <dbl>     <dbl>      <dbl>
#> 1 model      0.01       0.208     0.208          1
#> 2 model      0.02       0.200     0.200          1
#> 3 model      0.03       0.192     0.192          1
#> 4 model      0.04       0.183     0.183          1
#> 5 model      0.05       0.175     0.175          1
#> 6 model      0.06       0.166     0.166          1
```
