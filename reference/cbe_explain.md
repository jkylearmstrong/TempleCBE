# Explain Joint Model Components via DALEX and survex

Extracts and creates explainers for all components of a `joint_model`:
the survival model (via `survex`), the binary status model (via
`DALEX`), and the continuous duration time model (via `DALEX`).

## Usage

``` r
cbe_explain(
  model,
  data = NULL,
  type = c("all", "survival", "status", "time", "stack"),
  times = NULL,
  label = NULL,
  verbose = FALSE,
  ...
)

cbe_explain_status(model, data = NULL, label = NULL, verbose = FALSE, ...)

cbe_explain_time(model, data = NULL, label = NULL, verbose = FALSE, ...)

cbe_explain_stack(model, data = NULL, label = NULL, verbose = FALSE, ...)

explain_joint(
  model,
  data = NULL,
  type = c("all", "survival", "status", "time", "stack"),
  times = NULL,
  label = NULL,
  verbose = FALSE,
  ...
)

explain_status(model, data = NULL, label = NULL, verbose = FALSE, ...)

explain_time(model, data = NULL, label = NULL, verbose = FALSE, ...)

explain_stack(model, data = NULL, label = NULL, verbose = FALSE, ...)
```

## Arguments

- model:

  A fitted `joint_model` object.

- data:

  Evaluation data frame with the original predictor columns (before
  one-hot encoding; extra columns are ignored). If `NULL`, the training
  predictors are used, as the original columns. The status and time
  explainers re-apply the model's training encoding with
  [`forge`](https://hardhat.tidymodels.org/reference/forge.html), as
  [`predict.joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/predict.joint_model.md)
  does, so a factor predictor is one column in the explainers too.

- type:

  Character indicating which component(s) to explain: `"all"`,
  `"survival"`, `"status"`, `"time"`, or `"stack"`.

- times:

  Numeric vector of evaluation time points for survival explanations.

- label:

  Optional custom label prefix.

- verbose:

  Logical; if `TRUE`, progress messages are shown. Default is `FALSE`.

- ...:

  Additional arguments passed to the underlying explainers.

## Value

If `type = "all"`, a list of class `"cbe_joint_explainer"` containing
`$survival`, `$status`, `$time`, and (if present) `$stack` explainers.
Otherwise, the individual requested explainer.
