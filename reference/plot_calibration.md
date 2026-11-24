# Calibration Plot of Predicted Risks

Plots the mean predicted risk of each bin against the observed event
rate, with a confidence interval, the diagonal of perfect calibration,
and the calibration slope and intercept of each model in the subtitle
(binary outcomes). See \[calibration_table()\] for the definitions, the
Surv outcomes and the handling of competing events.

## Usage

``` r
plot_calibration(
  truth,
  pred,
  horizon = NULL,
  bins = 10,
  strategy = c("quantile", "uniform"),
  event_level = c("second", "first"),
  conf_level = 0.95,
  smooth = FALSE
)
```

## Arguments

- truth:

  The outcome: 0/1, logical, a two-level factor, or a right-censored
  \[survival::Surv()\] object.

- pred:

  Predicted risks (probabilities of the event, by \`horizon\` for Surv
  outcomes): a numeric vector, or a named list or data frame with one
  element per model.

- horizon:

  For a Surv \`truth\`, the time at which the predicted risks apply.

- bins:

  Number of bins (default \`10\`).

- strategy:

  \`"quantile"\` (default; bins of equal size) or \`"uniform"\`
  (equal-width bins of the predicted risk).

- event_level:

  For a factor \`truth\`: \`"second"\` (default, as \`glm()\`) or
  \`"first"\` level is the event.

- conf_level:

  Level of the confidence intervals (default \`0.95\`): Wilson for
  binary, log-log Kaplan-Meier for Surv outcomes.

- smooth:

  Also draw a loess smooth of the observed event on the predicted risk,
  for a binary outcome (default \`FALSE\`).

## Value

A \`ggplot2\` object.

## See also

\[calibration_table()\], \[decision_curve()\]

## Examples

``` r
set.seed(1)
p <- runif(500)
y <- rbinom(500, 1, p)
plot_calibration(y, list(correct = p, overconfident = plogis(2 * qlogis(pmin(pmax(p, .01), .99)))))
```
