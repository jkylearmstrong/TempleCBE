# Multivariable Cox Proportional Hazards Modeling Engine

Fits a multivariable Cox proportional hazards model, validates the
proportional hazards assumption for every term (including the
multivariate global test), formats coefficients with explicit reference
levels grouped by variable, and gathers model fit and convergence
metrics.

## Usage

``` r
cbe_cox_multi(
  data,
  formula = NULL,
  outcome = "outcome",
  features = NULL,
  conf_level = 0.95,
  ...
)
```

## Arguments

- data:

  A data frame containing survival data and the candidate features.

- formula:

  Optional. A model formula with a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  outcome, e.g. `Surv(time, status) ~ age + sex`. When supplied,
  `outcome` and `features` are ignored.

- outcome:

  Character string naming the Surv object or outcome column in `data`
  (e.g., `"outcome"` or `"Surv(survival_time, status)"`). Ignored if
  `formula` is supplied.

- features:

  Character vector of candidate predictor columns in `data`. Required if
  `formula` is not supplied.

- conf_level:

  Numeric confidence level (default: 0.95).

- ...:

  Additional arguments passed to
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  (e.g., `id`, `ties`, `weights`).

## Value

An object of class `cbe_cox_multi` containing:

- `model`: The fitted
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  object.

- `table`: Tidy coefficient table (HR, log-HR, CI, p-value) with
  explicit reference rows per factor, grouped by `Variable`.

- `glance`: One-row data frame of model goodness-of-fit metrics.

- `zph`: A `cbe_cox_check` object (includes the multivariate global
  test).

- `converged`: Logical; whether the fit converged within the iteration
  limit.

- `n_iterations`: Number of Newton-Raphson iterations used by the fit.

- `features`: Character vector of feature names used.

- `var_labels`: Named character vector mapping feature names to labels.

## See also

\[cbe_cox_single()\], \[cbe_cox_check()\], \[cbe_cox_table()\],
\[plot_cox_forest_multi()\]
