# Compare the Fit of Several Cox Models in One Table

One row per model with the number of subjects and events,
log-likelihood, AIC, BIC and concordance (with its standard error),
taken from \[broom::glance()\]. Use it to line up candidate survival
specifications.

## Usage

``` r
tbl_cox_glance_compare(
  models,
  names = NULL,
  sort_by = c("AIC", "BIC", "concordance"),
  digits = 2
)
```

## Arguments

- models:

  A list of \[survival::coxph()\] fits, or of \[cox_single()\] /
  \[cox_multi()\] results (their \`model\` is used). A single model is
  accepted.

- names:

  Optional model names (one per model). By default the names of
  \`models\`, otherwise \`"Model 1"\`, \`"Model 2"\`, ...

- sort_by:

  \`"AIC"\` (default) or \`"BIC"\` (smallest first), \`"concordance"\`
  (largest first) or \`NULL\` to keep the order of \`models\`.

- digits:

  Decimals for the log-likelihood, AIC, BIC and concordance.

## Value

A tibble of class \`cbe_cox_glance\` with text columns \`Model\`, \`N\`,
\`Events\`, \`Log-Likelihood\`, \`AIC\`, \`BIC\` and \`Concordance
(SE)\`, and the attribute \`"numeric"\` holding the unformatted values
(\`model\`, \`n\`, \`events\`, \`loglik\`, \`AIC\`, \`BIC\`,
\`concordance\`, \`se_concordance\`).

## Comparability

AIC and BIC are only comparable between models fitted to the \*\*same
rows\*\*. The function warns when the models were fitted to different
numbers of subjects or events (usually because a predictor had missing
values in one model and not in another).

## See also

\[cox_table()\], \[cox_single()\], \[cox_multi()\]

## Examples

``` r
lung <- survival::lung
m1 <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
m2 <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
tbl_cox_glance_compare(list(age = m1, `age + sex` = m2))
#> # A tibble: 2 × 7
#>   Model     N     Events `Log-Likelihood` AIC     BIC     `Concordance (SE)`
#>   <chr>     <chr> <chr>  <chr>            <chr>   <chr>   <chr>             
#> 1 age + sex 228   165    -742.85          1489.70 1495.91 0.60 (0.03)       
#> 2 age       228   165    -747.79          1497.58 1500.68 0.55 (0.03)       
```
