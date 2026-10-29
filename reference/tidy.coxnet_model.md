# Tidy the Coefficients of a \`coxnet\` Model

Tidy the Coefficients of a \`coxnet\` Model

## Usage

``` r
# S3 method for class 'coxnet_model'
tidy(x, penalty = NULL, ...)
```

## Arguments

- x:

  A \[coxnet()\] model.

- penalty:

  The penalty to report coefficients at; defaults to the model's
  \`penalty\`.

- ...:

  Not used.

## Value

A tibble with \`term\`, \`estimate\` (log hazard ratio per unit of the
predictor; 0 for predictors the penalty removed), and \`penalty\`.
