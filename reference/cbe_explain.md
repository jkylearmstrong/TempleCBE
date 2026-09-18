# Explain Joint Model Components via DALEX and survex

Extracts and creates explainers for all components of a `joint_model`:
the survival model (via `survex`), the binary status model (via
`DALEX`), and the continuous duration time model (via `DALEX`).

## Usage

``` r
cbe_explain(
  model,
  data = NULL,
  type = c("all", "survival", "status", "time"),
  times = NULL,
  label = NULL,
  verbose = FALSE,
  ...
)

cbe_explain_status(model, data = NULL, label = NULL, verbose = FALSE, ...)

cbe_explain_time(model, data = NULL, label = NULL, verbose = FALSE, ...)
```

## Arguments

- model:

  A fitted `joint_model` object.

- data:

  Evaluation data frame. If `NULL`, training predictors are used.

- type:

  Character indicating which component(s) to explain: `"all"`,
  `"survival"`, `"status"`, or `"time"`.

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
`$survival`, `$status`, and `$time` explainers. Otherwise, the
individual requested explainer.
