# Survival Cutpoints With a Search-Adjusted P-Value (\`surv_cutpoint_condmc\`)

The maximally selected log-rank cutpoint of each variable, as
\`survminer::surv_cutpoint()\` finds it, with the p-value that
\`survminer\` does not report: \`maxstat::maxstat.test()\` with
\`pmethod = "condMC"\` (by default), which corrects for having searched
over every cutpoint. (\`survminer\` fixes \`pmethod = "none"\`.) For the
full set of selection rules and plots of one variable, see
\[km_cutpoint_search()\].

## Usage

``` r
surv_cutpoint_condmc(
  data,
  time,
  event,
  variables,
  minprop = 0.1,
  pmethod = c("condMC", "none", "Lau92", "Lau94", "exactGauss", "HL", "min"),
  B = 10000,
  seed = NULL
)
```

## Arguments

- data:

  A data frame.

- time, event:

  Names of the follow-up time and event status (1 = event, 0 = censored)
  columns.

- variables:

  Names of the continuous variables to cut.

- minprop:

  Minimum proportion of subjects in each group (default 0.1).

- pmethod:

  P-value method of \`maxstat::maxstat.test()\` (default \`"condMC"\`);
  \`"none"\` gives no p-value.

- B:

  Number of Monte Carlo replications for \`"condMC"\` (default 10000).

- seed:

  Optional seed for the Monte Carlo p-values, set locally (the session's
  random number stream is left as it was).

## Value

A tibble with one row per variable: \`variable\`, \`cutpoint\`,
\`statistic\` (the standardized log-rank statistic), \`p_value\`,
\`pmethod\`, \`n_low\` and \`n_high\` (subjects at or below, and above,
the cutpoint).

## See also

\[km_cutpoint_search()\]

## Examples

``` r
if (requireNamespace("maxstat", quietly = TRUE)) {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "meal.cal")])
  lung$status <- lung$status - 1
  surv_cutpoint_condmc(lung, "time", "status", c("age", "meal.cal"), B = 2000, seed = 1)
}
#> # A tibble: 2 × 7
#>   variable cutpoint statistic p_value pmethod n_low n_high
#>   <chr>       <dbl>     <dbl>   <dbl> <chr>   <int>  <int>
#> 1 age            64      1.97   0.298 condMC     99     82
#> 2 meal.cal      488      1.38   0.702 condMC     29    152
```
