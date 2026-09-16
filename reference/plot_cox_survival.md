# Model-Predicted Survival Curves Stratified by Predictor

Generates stratified predicted survival curves from a Cox model. For
continuous predictors, values are automatically binned into quantile
strata (e.g. quartiles).

## Usage

``` r
plot_cox_survival(
  fit,
  data,
  feature = NULL,
  id_col = NULL,
  n_tiles = 4,
  label_endpoints = TRUE,
  base_size = 12
)
```

## Arguments

- fit:

  A fitted
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) model
  or a `cbe_cox` object.

- data:

  Data frame used for fitting the model.

- feature:

  Character name of the predictor column.

- id_col:

  Character name of subject ID column (default: "arl_number" or row
  number).

- n_tiles:

  Number of quantile bins for continuous predictors (default: 4).

- label_endpoints:

  Logical; if TRUE, repels labels for stratum values at the final time
  point.

- base_size:

  Base font size (default: 12).

## Value

A ggplot2 object.
