# Marginal Event Probability / Relative Hazard Diagnostic Plot

Plots a continuous predictor against a model-derived curve, with a 95
for `scale = "prob"`, overlaid actual binary event observations.

## Usage

``` r
plot_cox_marginal(
  fit,
  data,
  feature = NULL,
  status_col = "status",
  scale = c("prob", "hr", "log_hr"),
  color = "#9D2235",
  base_size = 12
)
```

## Arguments

- fit:

  A fitted
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) model
  or a `cbe_cox` object.

- data:

  Data frame containing survival inputs.

- feature:

  Character name of continuous predictor column.

- status_col:

  Character name of status/event column (0 = censored, 1 = event;
  default: "status"). Only used when `scale = "prob"`.

- scale:

  One of `"prob"` (default; predicted probability of the event), `"hr"`
  (relative hazard, i.e. [`exp()`](https://rdrr.io/r/base/Log.html) of
  the centered linear predictor), or `"log_hr"` (the centered linear
  predictor itself).

- color:

  Primary color for the fitted curve and CI band (default: Temple Cherry
  `"#9D2235"`).

- base_size:

  Base font size (default: 12).

## Value

A ggplot2 object.
