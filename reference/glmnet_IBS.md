# Integrated Brier Score (IBS) Evaluation for Regularized Survival Models

Computes the Integrated Brier Score (IBS) for regularized Cox
proportional hazards models fit via cross-validated \`glmnet\`. Supports
resampling split objects (e.g., \`rsplit\` from \`rsample\`) as well as
data frames.

## Usage

``` r
glmnet_IBS(object, alpha = 1, formula = NULL, cox.ties = "efron")
```

## Arguments

- object:

  A resampling fold object (e.g. \`rsplit\` from \`rsample\`) or a data
  frame/tibble.

- alpha:

  The elastic net mixing parameter: 1 for Lasso, 0 for Ridge (default
  1).

- formula:

  Optional formula specifying target survival response and predictor
  features.

- cox.ties:

  Method for handling ties in Cox model, passed to \`glmnet::cv.glmnet\`
  (default \`"efron"\`).

## Value

A tibble containing \`IBS\`, optimal \`lambda\`, and \`alpha\`.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("glmnet", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
  df <- data.frame(
    time = runif(60, 5, 100),
    status = rbinom(60, 1, 0.7),
    age = rnorm(60, 50, 10),
    bmi = rnorm(60, 25, 4)
  )
  glmnet_IBS(df)
}
} # }
```
