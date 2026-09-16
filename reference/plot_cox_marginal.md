# Marginal Event Probability Diagnostic Plot

Plots predicted probability of the event vs. a continuous predictor,
overlaid with actual binary event observations and smoothed confidence
intervals.

## Usage

``` r
plot_cox_marginal(
  fit,
  data,
  feature = NULL,
  status_col = "status",
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
  default: "status").

- base_size:

  Base font size (default: 12).

## Value

A ggplot2 object.
