# Hazard Ratio of a Time-Varying Coefficient at Given Times

For a time-varying predictor \`x\` with log hazard ratio \\\beta +
\gamma f(t)\\, returns \\\exp(\beta + \gamma f(t))\\ at each time in
\`times\`, with confidence intervals from the delta method (the
covariance of the two coefficients comes from the fitted model).

## Usage

``` r
hazard_ratio_at_time(object, variable, times)
```

## Arguments

- object:

  A \[cox_time_varying()\] fit.

- variable:

  One time-varying predictor name (an element of \`object\$tv_vars\`).

- times:

  Numeric vector of times to report the hazard ratio at. Each time must
  give a finite value of the fitted \`time_fn\`: with \`time_fn =
  "log"\`, \`t = 0\` gives \`log(0) = -Inf\` and is an error, not a
  hazard ratio of \`Inf\`/\`NaN\`.

## Value

A tibble with \`variable\`, \`time\`, \`hazard_ratio\`, \`conf_low\` and
\`conf_high\`.

## See also

\[cox_time_varying()\]

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
  hazard_ratio_at_time(fit, "age", times = c(10, 50, 100))
}
#> # A tibble: 3 × 5
#>   variable  time hazard_ratio conf_low conf_high
#>   <chr>    <dbl>        <dbl>    <dbl>     <dbl>
#> 1 age         10         1.01    0.981      1.03
#> 2 age         50         1.01    0.994      1.03
#> 3 age        100         1.01    0.989      1.04
```
