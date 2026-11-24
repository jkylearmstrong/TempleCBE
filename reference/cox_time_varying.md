# Time-Varying-Coefficient Cox Model

Fits a Cox model in which the coefficients of \`tv_vars\` vary with
time: the log hazard ratio of a time-varying predictor \`x\` is
\\\beta + \gamma f(t)\\, with \`f\` the time function (\`log\` by
default), instead of a constant \`\\\beta\\\`. This is the usual remedy
when \[tbl_schoenfeld_audit()\] flags a proportional-hazards violation
for a term: fit the term with a time-varying coefficient rather than
dropping it or stratifying on it. The SAS counterpart is
\`inst/sas/coxtvc.sas\`.

## Usage

``` r
cox_time_varying(
  data,
  formula,
  tv_vars,
  time_fn = c("log", "identity", "sqrt"),
  conf_level = 0.95,
  ...
)
```

## Arguments

- data:

  A data frame with the outcome and predictors.

- formula:

  A two-sided formula with a \[survival::Surv()\] outcome, such as
  \`survival::Surv(time, status) ~ age + sex\`. Terms may not already
  use \`tt()\`; name time-varying predictors in \`tv_vars\` instead.

- tv_vars:

  Character vector of predictor names whose coefficients vary with time.
  Each must be a term of \`formula\` and a numeric column of \`data\`.

- time_fn:

  The time function \`f(t)\`: \`"log"\` (default, \`log(t)\`),
  \`"identity"\` (\`t\`), \`"sqrt"\` (\`sqrt(t)\`), or a function of one
  numeric vector. It must return finite values at the observed event
  times, so \`"log"\` needs event times above zero.

- conf_level:

  Confidence level of the hazard ratios (default \`0.95\`).

- ...:

  Further arguments passed to \[survival::coxph()\], such as
  \`weights\`. \`ties\` may be passed and overrides the default
  \`"efron"\`. \`tt\`, \`model\`, \`x\` and \`y\` are set by this
  function itself: \`tt\` passed via \`...\` is an error, and
  \`model\`/\`x\`/\`y\` passed via \`...\` are ignored with a warning.

## Value

An object of class \`cox_time_varying\`: a list with \`fit\` (the
\[survival::coxph()\] fit), \`terms\` (a tibble with \`term\`,
\`estimate\`, \`std_error\`, \`hr\`, \`conf_low\`, \`conf_high\`,
\`statistic\`, \`p_value\` and \`time_varying\`), \`tv_vars\`,
\`time_fn\` (the name of the time function, or \`"user function"\`),
\`n\`, \`n_events\`, and \`conf_level\`. \`tidy()\` returns \`terms\`;
\`predict()\` gives linear predictors at a time or survival
probabilities.

## Details

The fit is \[survival::coxph()\] with a \`tt()\` term, so the model is
an ordinary Cox fit with one extra term per time-varying predictor: the
coefficient on \`x\` is its log hazard ratio where \`f(t) = 0\` (at \`t
= 1\` for \`time_fn = "log"\`), and the coefficient on \`tt(x)\` is how
that log hazard ratio changes per unit of \`f(t)\`. Use
\[hazard_ratio_at_time()\] for the hazard ratio at a given time.

\*\*Limits of this draft.\*\* Time-varying predictors must be numeric
columns; a factor or interaction term cannot be a \`tv_var\`.
\`strata()\` terms are not supported. Survival predictions extrapolate
flat past the last observed event time (the cumulative baseline hazard
is constant there), as with a Breslow estimate. The linear predictor
varies with time, so \`predict(type = "linear_pred")\` needs one
\`eval_time\`.

## See also

\[tbl_schoenfeld_audit()\], \[hazard_ratio_at_time()\],
\[recurrent_cox()\], \[cox_single()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  set.seed(1)
  n <- 200
  d <- data.frame(
    time = stats::rexp(n, 0.02), status = stats::rbinom(n, 1, 0.7),
    age = stats::rnorm(n, 60, 10), sex = stats::rbinom(n, 1, 0.5)
  )
  fit <- cox_time_varying(d, survival::Surv(time, status) ~ age + sex, tv_vars = "age")
  fit
  hazard_ratio_at_time(fit, "age", times = c(10, 50, 100))
}
#> # A tibble: 3 × 5
#>   variable  time hazard_ratio conf_low conf_high
#>   <chr>    <dbl>        <dbl>    <dbl>     <dbl>
#> 1 age         10         1.01    0.981      1.03
#> 2 age         50         1.01    0.994      1.03
#> 3 age        100         1.01    0.989      1.04
```
