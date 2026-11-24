# Decision Curve Analysis: Net Benefit of a Risk Model

Net benefit of acting on patients whose predicted risk is at or above a
threshold probability, \`TP/n - FP/n \* pt / (1 - pt)\`, over a range of
thresholds \`pt\`, against the strategies "treat all" and "treat none"
(net benefit 0) (Vickers and Elkin, 2006). A model is useful at the
thresholds where its curve lies above both. For a right-censored outcome
(\`horizon\`), the event rate in the group above the threshold is the
Kaplan-Meier risk at the horizon, as in Vickers et al. (2008); competing
events are treated as censoring.

## Usage

``` r
decision_curve(
  truth,
  pred,
  horizon = NULL,
  thresholds = seq(0.01, 0.99, by = 0.01),
  event_level = c("second", "first")
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

- thresholds:

  Threshold probabilities, strictly between 0 and 1 (default \`seq(0.01,
  0.99, by = 0.01)\`).

- event_level:

  For a factor \`truth\`: \`"second"\` (default, as \`glm()\`) or
  \`"first"\` level is the event.

## Value

A tibble of class \`decision_curve\` with one row per model and
threshold: \`model\`, \`threshold\`, \`net_benefit\`, \`treat_all\` (net
benefit of treating everyone) and \`prop_above\` (the share of patients
at or above the threshold). Treat none is 0.

## References

Vickers AJ, Elkin EB (2006). Decision curve analysis: a novel method for
evaluating prediction models. Medical Decision Making 26:565-574.
Vickers AJ, Cronin AM, Elkin EB, Gonen M (2008). Extensions to decision
curve analysis, a novel method for evaluating diagnostic tests,
prediction models and molecular markers. BMC Medical Informatics and
Decision Making 8:53.

## See also

\[plot_decision_curve()\], \[calibration_table()\]

## Examples

``` r
set.seed(1)
x <- rnorm(400)
y <- rbinom(400, 1, plogis(-1 + x))
fit <- glm(y ~ x, family = binomial())
head(decision_curve(y, fitted(fit)))
#> # A tibble: 6 × 5
#>   model threshold net_benefit treat_all prop_above
#>   <chr>     <dbl>       <dbl>     <dbl>      <dbl>
#> 1 model      0.01       0.303     0.303      1    
#> 2 model      0.02       0.296     0.296      1    
#> 3 model      0.03       0.289     0.289      1    
#> 4 model      0.04       0.281     0.281      0.998
#> 5 model      0.05       0.274     0.274      0.995
#> 6 model      0.06       0.267     0.266      0.988
```
