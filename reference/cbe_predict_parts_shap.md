# SHAP Variable Attributions across Survival and Joint Models

Computes individual observation SHAP attributions: using time-dependent
`SurvSHAP(t)` via `survex::predict_parts(type = "survshap")` for
survival explainers, and `DALEX::predict_parts(type = "shap")` for
classification/regression explainers.

## Usage

``` r
cbe_predict_parts_shap(
  explainer,
  new_observation,
  type = c("auto", "shap", "survshap"),
  ...
)
```

## Arguments

- explainer:

  An explainer object produced by
  [`cbe_explain_survival()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_explain_survival.md),
  [`cbe_explain()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_explain.md),
  or
  [`DALEX::explain()`](https://modeloriented.github.io/DALEX/reference/explain.html).

- new_observation:

  A 1-row data frame containing the observation to explain.

- type:

  Character specifying the attribution type: `"auto"` (the default),
  `"shap"`, or `"survshap"`.

- ...:

  Additional arguments passed to
  [`survex::predict_parts()`](https://modeloriented.github.io/survex/reference/predict_parts.surv_explainer.html)
  or
  [`DALEX::predict_parts()`](https://modeloriented.github.io/DALEX/reference/predict_parts.html).

## Value

A `predict_parts` attribution object, printable and plottable with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).
