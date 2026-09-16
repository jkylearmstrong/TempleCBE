# Univariable Cox Proportional Hazards Screening Engine

Fits a univariable Cox proportional hazards model, validates the
proportional hazards assumption, formats coefficients with explicit
reference levels, generates plain-language clinical interpretations, and
gathers model fit metrics.

## Usage

``` r
cbe_cox_single(data, outcome = "outcome", feature, conf_level = 0.95, ...)
```

## Arguments

- data:

  A data frame containing survival data and the candidate feature.

- outcome:

  Character string naming the Surv object or outcome column in `data`
  (e.g., `"outcome"` or `"Surv(survival_time, status)"`).

- feature:

  Character string naming the candidate predictor column in `data`.

- conf_level:

  Numeric confidence level (default: 0.95).

- ...:

  Additional arguments passed to
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  (e.g., `weights`, `ties`).

## Value

An object of class `cbe_cox` containing:

- `model`: The fitted
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  object.

- `zph`: The
  [`survival::cox.zph`](https://rdrr.io/pkg/survival/man/cox.zph.html)
  proportional hazards test object.

- `zph_table`: Formatted assumption test table.

- `zph_violated`: Logical indicating whether the assumption was violated
  (p \< 0.05).

- `zph_text`: Automated sentence summarizing the assumption test.

- `table`: Clean coefficient table with explicit reference level rows
  for factors.

- `interpretation`: Plain-language automated interpretation of the
  hazard ratio(s).

- `glance`: One-row data frame of model goodness-of-fit metrics.

- `feature`: Feature name string.

- `var_label`: Human-readable variable label.

- `is_numeric`: Logical flag indicating whether predictor is
  continuous/numeric.

## See also

\[cbe_cox_multi()\], \[cbe_cox_check()\], \[cbe_km_single()\],
\[cbe_cox_table()\], \[plot_cox_forest()\], \[plot_cox_survival()\],
\[plot_cox_marginal()\]
