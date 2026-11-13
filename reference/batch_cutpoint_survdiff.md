# Scan Cutpoints of Several Features with the Log-Rank Test

For every feature, dichotomises the cohort at each candidate cutpoint
(\`\<= cutpoint\` is "low", \`\> cutpoint\` "high"), runs
\[survival::survdiff()\] and returns the p-values with a significance
bin. Candidate cutpoints are the midpoints between consecutive distinct
values or quantiles. The univariate Cox coefficient is returned too,
which \[first_significant_cutpoint()\] uses to pick the first
significant cutpoint.

## Usage

``` r
batch_cutpoint_survdiff(
  data,
  time_col,
  status_col,
  feature_cols,
  cutpoints = c("midpoints", "quantiles"),
  probs = seq(0.1, 0.9, by = 0.1),
  min_n = 1,
  p_breaks = c(0.001, 0.05, 0.1)
)
```

## Arguments

- data:

  A data frame.

- time_col, status_col:

  Names of the follow-up time and event (1 = event) columns.

- feature_cols:

  Names of the numeric features to scan.

- cutpoints:

  \`"midpoints"\` (default) or \`"quantiles"\`.

- probs:

  Quantile probabilities for \`cutpoints = "quantiles"\`.

- min_n:

  Minimum number of subjects in each of the two groups (default 1).

- p_breaks:

  Increasing p-value breaks for the significance bin (default \`c(0.001,
  0.05, 0.1)\`).

## Value

A tibble with \`feature\`, \`cutpoint\`, \`n_low\`, \`n_high\`,
\`chisq\`, \`p\`, \`p_type\` (ordered factor: \`"p \>= 0.1"\`, \`"p \<
0.1"\`, \`"p \< 0.05"\`, \`"p \< 0.001"\` for the default breaks),
\`cox_coef\` and \`cox_p\`. A feature without a usable cutpoint
contributes no rows.

## See also

\[first_significant_cutpoint()\], \[km_cutpoint_search()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(80), status = rbinom(80, 1, 0.7), x = rnorm(80), z = rnorm(80))
res <- batch_cutpoint_survdiff(d, "time", "status", c("x", "z"), cutpoints = "quantiles")
head(res)
#> # A tibble: 6 × 9
#>   feature cutpoint n_low n_high   chisq     p p_type   cox_coef cox_p
#>   <chr>      <dbl> <int>  <int>   <dbl> <dbl> <ord>       <dbl> <dbl>
#> 1 x        -1.11       8     72 1.02    0.313 p >= 0.1  0.00479 0.975
#> 2 x        -0.949     16     64 0.00354 0.953 p >= 0.1  0.00479 0.975
#> 3 x        -0.714     24     56 0.00167 0.967 p >= 0.1  0.00479 0.975
#> 4 x        -0.438     32     48 0.0322  0.858 p >= 0.1  0.00479 0.975
#> 5 x        -0.160     40     40 0.215   0.643 p >= 0.1  0.00479 0.975
#> 6 x         0.0963    48     32 0.0359  0.850 p >= 0.1  0.00479 0.975
```
