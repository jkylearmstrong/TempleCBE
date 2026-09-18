# Explain Survival Models via survex and DALEX

Adapts Tidymodels survival models (`workflows`, `parsnip` / `censored`,
`coxnet_model`, and `joint_model`) to create a `survex` survival
explainer. Standardizes survival predictions into \\N \times K\\
survival curves, bridging the gap identified in survex issue \#98.

## Usage

``` r
cbe_explain_survival(
  model,
  data = NULL,
  y = NULL,
  predict_survival_function = NULL,
  predict_risk_function = NULL,
  times = NULL,
  label = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- model:

  A fitted model object: `joint_model`, `coxnet_model`, `cv_coxnet`,
  `workflow`, or `model_fit`.

- data:

  A data frame containing predictor columns used to calculate
  explanations. If `NULL` and `model` is a `joint_model`, the training
  predictors are used.

- y:

  A survival object or response variable (e.g. `Surv(time, status)`). If
  `NULL` and `model` is a `joint_model`, the original survival object is
  used.

- predict_survival_function:

  Optional custom prediction function returning an \\N \times K\\ data
  frame or matrix of survival probabilities \\S(t)\\.

- predict_risk_function:

  Optional custom prediction function returning a numeric vector of risk
  scores or cumulative hazard.

- times:

  A numeric vector of evaluation time points. If `NULL`, defaults to
  unique event times in `y`.

- label:

  Character string naming the model in plots and summaries.

- verbose:

  Logical; if `TRUE`, progress messages are shown. Default is `FALSE`.

- ...:

  Additional arguments passed to
  [`survex::explain_survival()`](https://modeloriented.github.io/survex/reference/explain_survival.html).

## Value

A `survex_explainer` object compatible with
[`survex::model_parts()`](https://modeloriented.github.io/survex/reference/model_parts.surv_explainer.html),
[`survex::model_performance()`](https://modeloriented.github.io/survex/reference/model_performance.surv_explainer.html),
and
[`survex::predict_parts()`](https://modeloriented.github.io/survex/reference/predict_parts.surv_explainer.html).

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("survex", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
  lung <- survival::lung
  df <- na.omit(lung[, c("time", "status", "age", "sex")])
  fit <- joint_model(df, survival::Surv(time, status) ~ age + sex)
  expl <- cbe_explain_survival(fit)
  mp <- survex::model_parts(expl)
}
} # }
```
