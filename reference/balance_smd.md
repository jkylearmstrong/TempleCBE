# Standardized Mean Differences for Covariate Balance

Computes the standardized mean difference (SMD) of each covariate
between a group and a reference group, before and, when \`weights\` are
given, after weighting (inverse probability of treatment, overlap or
matching weights). It is the table behind a Love plot,
\[plot_balance_love()\], and the check that comes with a
propensity-score analysis or a table-one of two cohorts.

## Usage

``` r
balance_smd(
  data,
  group,
  vars = NULL,
  weights = NULL,
  ref = NULL,
  denominator = c("pooled", "reference"),
  threshold = 0.1
)
```

## Arguments

- data:

  A data frame.

- group:

  Name of the grouping column: two or more levels.

- vars:

  Covariates (column names). Default: every column except \`group\` and
  the \`weights\` column.

- weights:

  \`NULL\` (default), the name of a numeric column of \`data\`, or a
  numeric vector with one weight per row. When given, the table has an
  \`"Adjusted"\` row besides the \`"Unadjusted"\` one for each
  covariate. Rows with a missing weight are dropped from the Adjusted
  sample only; the Unadjusted sample keeps them.

- ref:

  The reference level of \`group\`. Default: the first level (the first
  value, for a non-factor).

- denominator:

  \`"pooled"\` (default) or \`"reference"\` (the standard deviation of
  the reference group, as for the average treatment effect on the
  treated).

- threshold:

  The \`abs(smd)\` below which a covariate is called balanced (default
  \`0.1\`).

## Value

A tibble of class \`balance_smd\` with one row per comparison, covariate
and sample: \`comparison\` (\`"level vs ref"\`), \`variable\`,
\`sample\` (\`"Unadjusted"\` / \`"Adjusted"\`), \`mean_ref\`,
\`mean_cmp\`, \`smd\`, \`abs_smd\`, \`var_ratio\` (variance in the
compared group over the reference group) and \`balanced\`. The reference
level and the threshold are in \`attr(x, "ref")\` and \`attr(x,
"threshold")\`.

## Details

For a numeric covariate the SMD is the difference in means over the
pooled standard deviation, \`sqrt((s_ref^2 + s_cmp^2) / 2)\`. A logical
or 0/1 covariate is treated as numeric, so its SMD is the difference in
proportions over \`sqrt((v_ref + v_cmp) / 2)\` with the sample variance.
A factor or character covariate becomes one 0/1 indicator per level
(named \`variable = level\`). Variances are the unbiased (sample) ones.
With weights, the means in the numerator are weighted but the standard
deviation in the denominator is that of the unweighted sample, so the
adjusted and unadjusted SMDs are on the same scale (as in \`cobalt\` and
\`MatchIt\`); \`var_ratio\` uses the weighted variances. Missing values
are dropped covariate by covariate. A common rule of thumb calls
\`abs(smd) \< 0.1\` balanced.

## See also

\[plot_balance_love()\]. The \`cobalt\` package has a fuller set of
balance tools (balance after matching and weighting, many weighting
estimands); this function needs no extra package and covers the table
and the Love plot.

## Examples

``` r
set.seed(1)
d <- data.frame(
  treated = rep(c("no", "yes"), each = 100),
  age = c(rnorm(100, 60, 10), rnorm(100, 66, 10)),
  male = c(rbinom(100, 1, 0.45), rbinom(100, 1, 0.6)),
  site = sample(c("A", "B", "C"), 200, replace = TRUE)
)
balance_smd(d, "treated")
#> Covariate balance (reference: no); 3 covariate row(s) with |SMD| >= 0.1 in the unadjusted sample
#> # A tibble: 5 × 9
#>   comparison variable sample     mean_ref mean_cmp     smd abs_smd var_ratio
#>   <chr>      <chr>    <chr>         <dbl>    <dbl>   <dbl>   <dbl>     <dbl>
#> 1 yes vs no  age      Unadjusted    61.1     65.6   0.488   0.488      1.14 
#> 2 yes vs no  male     Unadjusted     0.42     0.53  0.221   0.221      1.02 
#> 3 yes vs no  site = A Unadjusted     0.31     0.37  0.126   0.126      1.09 
#> 4 yes vs no  site = B Unadjusted     0.36     0.34 -0.0417  0.0417     0.974
#> 5 yes vs no  site = C Unadjusted     0.33     0.29 -0.0861  0.0861     0.931
#> # ℹ 1 more variable: balanced <lgl>
```
