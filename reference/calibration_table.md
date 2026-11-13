# Calibration Table of Predicted Risks

Groups the predicted risks into bins and compares the mean predicted
risk of each bin with the observed event rate. A well-calibrated model
lies on the diagonal. Works for a binary outcome and, with \`horizon\`,
for a right-censored time-to-event outcome, where the observed risk of a
bin is one minus its Kaplan-Meier survival at \`horizon\`. Competing
risks are not accounted for: a competing event is treated as censoring
in the Kaplan-Meier estimate, which overstates the observed risk when
competing events are common.

## Usage

``` r
calibration_table(
  truth,
  pred,
  horizon = NULL,
  bins = 10,
  strategy = c("quantile", "uniform"),
  event_level = c("second", "first"),
  conf_level = 0.95
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

## Value

A tibble with one row per model and bin: \`model\`, \`bin\`, \`n\`,
\`mean_pred\`, \`observed\`, \`conf_low\`, \`conf_high\`. \`attr(x,
"metrics")\` is a tibble with \`model\`, \`n\`, \`intercept\`, \`slope\`
and \`brier\`.

## Details

For a binary outcome the table also has the calibration intercept
(calibration-in-the-large, with the logit of the predicted risk as an
offset) and slope (the coefficient of the logit of the predicted risk),
and the Brier score, in \`attr(x, "metrics")\`. A slope below 1 means
the predictions are too extreme (overfitting). These are \`NA\` for Surv
outcomes, and also \`NA\` when the binary outcome has a single class
(the logistic fits are then degenerate: separation makes the
intercept/slope essentially arbitrary).

## See also

\[plot_calibration()\], \[decision_curve()\]

## Examples

``` r
set.seed(1)
p <- runif(500)
y <- rbinom(500, 1, p)
calibration_table(y, p, bins = 5)
#> # A tibble: 5 × 7
#>   model   bin     n mean_pred observed conf_low conf_high
#>   <chr> <int> <int>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 model     1   100     0.114     0.07   0.0343     0.137
#> 2 model     2   100     0.303     0.36   0.273      0.458
#> 3 model     3   100     0.473     0.45   0.356      0.548
#> 4 model     4   100     0.686     0.64   0.542      0.727
#> 5 model     5   100     0.902     0.89   0.814      0.937
attr(calibration_table(y, p), "metrics")
#> # A tibble: 1 × 5
#>   model     n intercept slope brier
#>   <chr> <int>     <dbl> <dbl> <dbl>
#> 1 model   500   -0.0805 0.986 0.172
```
