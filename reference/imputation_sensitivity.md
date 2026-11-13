# Sensitivity Analysis Across Imputation Strategies

Refits the same model under several imputation strategies and summarises
how its estimates move. The motivating case (fresh-review B4) is a
result that rests on one imputation choice — for example median
imputation of a variable that is 42 would change the conclusion. This
function makes that check explicit: one fit per strategy, one row per
term per strategy, and a per-term summary (minimum, maximum, range and
standard deviation of the estimate across strategies), plus a
Rubin's-rules pool (\[pool_imputations()\]) over the strategies when
every strategy reports the same terms.

## Usage

``` r
imputation_sensitivity(data, fit_fn, strategies, conf_level = 0.95, ...)

# S3 method for class 'imputation_sensitivity'
print(x, ...)
```

## Arguments

- data:

  A data frame with missing values.

- fit_fn:

  A function \`function(data)\` returning a fitted model. Its
  coefficients are extracted with \[broom::tidy()\] when that gives
  \`term\`, \`estimate\` and \`std.error\`, and otherwise from
  \[stats::coef()\] and the diagonal of \[stats::vcov()\].

- strategies:

  A named list of at least two strategies (see above). Names must be
  unique and non-empty.

- conf_level:

  Confidence level passed to \[pool_imputations()\] for the pooled
  table.

- ...:

  Passed to \[missforest_sweep_mtry()\] / \[missranger_sweep_mtry()\]
  for the \`"missforest"\` / \`"missranger"\` strategies.

- x:

  The object to print (the result of the function).

## Value

A list of class \`imputation_sensitivity\` with

- strategies:

  the strategy names, in order

- n_imputed:

  a tibble with \`strategy\`, \`n_imputed\` (missing cells filled;
  \`NA\` when a strategy reordered the rows and changed values, so its
  rows cannot be aligned with \`data\` without a row id; an observed
  numeric cell counts as unchanged when it is within 1e-8 (absolute for
  values below 1, relative above — that is, within \`1e-8 \* max(1,
  \|value\|)\`), and infinite cells must match exactly) and
  \`n_dropped\` (rows dropped; non-zero only for \`"complete_cases"\`,
  and never negative: rows a strategy adds are not counted)

- estimates:

  a tibble with \`term\`, \`strategy\`, \`estimate\` and \`std.error\`

- summary:

  one row per term: \`term\`, \`n_strategies\`, \`min\`, \`max\`,
  \`range\`, \`sd\`, \`median_se\`, and, when a pooled table exists,
  \`pooled_estimate\` and \`pooled_se\`

- pooled:

  the \[pool_imputations()\] table over the strategies, or \`NULL\` when
  the strategies do not report the same terms (with the reason in
  \`pooled_note\`)

- pooled_note:

  character string, or \`NULL\` when pooling succeeded

- fits:

  named list of the fitted model per strategy

## Details

A strategy is either a completed data frame, a function
\`function(data)\` returning a completed data frame, or one of the
built-in names:

- \`"complete_cases"\`:

  rows with any missing value dropped (\[stats::na.omit()\]); nothing is
  imputed.

- \`"median"\` / \`"mean"\`:

  numeric columns filled with their observed median / mean; factor and
  character columns with their mode.

- \`"missforest"\` / \`"missranger"\`:

  the whole data set imputed by \[missforest_sweep_mtry()\] /
  \[missranger_sweep_mtry()\] (needs the corresponding package); extra
  sweep arguments come from \`...\`.

## Reading the summary

A term whose estimate range across strategies is large relative to its
standard error (compare \`range\` with \`median_se\`) is sensitive to
the imputation choice: the reported result depends on how the missing
values were filled in, and the analysis should say so. The pooled
estimate treats the strategies as imputations for Rubin's rules; that is
a summary device, not a substitute for proper multiple imputation — the
strategies are chosen methods, not draws from a posterior predictive
distribution (see \[pool_imputations()\]).

## See also

\[pool_imputations()\], \[tune_impute_dataset()\],
\[missforest_sweep_mtry()\], \[missranger_sweep_mtry()\]

## Examples

``` r
d <- mtcars
d$hp[c(3, 11, 20)] <- NA
d$wt[c(5, 15)] <- NA
res <- imputation_sensitivity(
  d,
  fit_fn = function(x) stats::lm(mpg ~ wt + hp, data = x),
  strategies = list(median = "median", mean = "mean", complete_cases = "complete_cases")
)
res$summary
#> # A tibble: 3 × 9
#>   term    n_strategies     min     max   range      sd median_se pooled_estimate
#>   <chr>          <int>   <dbl>   <dbl>   <dbl>   <dbl>     <dbl>           <dbl>
#> 1 (Inter…            3 36.3    37.8    1.49    8.44e-1    2.05           37.3   
#> 2 wt                 3 -4.13   -3.60   0.534   3.01e-1    0.774          -3.95  
#> 3 hp                 3 -0.0319 -0.0309 0.00106 5.56e-4    0.0106         -0.0313
#> # ℹ 1 more variable: pooled_se <dbl>
```
