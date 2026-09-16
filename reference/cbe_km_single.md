# Univariable Kaplan-Meier and Cox Screening Engine

Pairs a univariable \[cbe_cox_single()\] Cox proportional hazards fit
with the matching Kaplan-Meier stratified survival curve for the same
predictor, so both are produced from a single call without refitting the
Cox model twice.

## Usage

``` r
cbe_km_single(data, outcome = "outcome", feature, conf_level = 0.95)
```

## Arguments

- data:

  A data frame containing survival data and the candidate feature.

- outcome:

  Character string naming the Surv object or outcome column in `data`
  (e.g., `"outcome"` or `"Surv(survival_time, status)"`).

- feature:

  Character string naming the candidate predictor column in `data`.
  Factor (or coercible) predictors are used directly as Kaplan-Meier
  strata; numeric predictors are binned into quartiles for the
  Kaplan-Meier strata (matching \[plot_cox_survival()\]'s convention),
  while the Cox side treats them as continuous.

- conf_level:

  Numeric confidence level (default: 0.95).

## Value

An object of class `cbe_km` containing:

- `cox`: The `cbe_cox` object from \[cbe_cox_single()\].

- `km_fit`: The
  [`survival::survfit`](https://rdrr.io/pkg/survival/man/survfit.html)
  object for the Kaplan-Meier curve.

- `km_tidy`:
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  table of `km_fit`.

- `direction`: `"increases"` or `"decreases"`, pulled from the Cox
  hazard ratio direction.

- `summary`: One-row-per-stratum tidy summary combining Cox hazard
  ratios and Kaplan-Meier median survival.

- `feature`: Feature name string.

- `var_label`: Human-readable variable label.

## See also

\[cbe_cox_single()\], \[plot_cox_survival()\]
