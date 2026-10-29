# Relative Squared Error of Imputation Methods Against a Null Baseline

Scores imputation methods on cells whose true value is known (values you
masked, then imputed). For each variable and method it computes the root
mean squared error, the RMSE of a \*\*null baseline\*\* that predicts
every cell with one overall value, and the \*\*relative squared
error\*\* \$\$\mathrm{RSE} = (\mathrm{RMSE}\_{\mathrm{method}} /
\mathrm{RMSE}\_{\mathrm{null}})^2.\$\$ An RSE below 1 beats the null
baseline, 1 matches it, above 1 is worse.

## Usage

``` r
compare_imputation_rse(
  results_df,
  null_method = c("mean", "median"),
  baseline = NULL,
  variable_col = "variable",
  method_col = "method",
  truth_col = "truth",
  imputed_col = "imputed"
)
```

## Arguments

- results_df:

  A data frame with one row per scored cell, with the columns named by
  \`variable_col\`, \`method_col\`, \`truth_col\` and \`imputed_col\`.
  Cells with a missing truth or imputed value are dropped and counted in
  \`n_dropped\`. Alternatively, a table that already has \`rmse\` and
  \`null_rmse\` columns per variable and method.

- null_method:

  \`"mean"\` (default) or \`"median"\`.

- baseline:

  Optional baseline values; see Null baseline.

- variable_col, method_col, truth_col, imputed_col:

  Column names in \`results_df\`.

## Value

A tibble of class \`cbe_rse\` with \`variable\`, \`method\`, \`n\`,
\`n_dropped\`, \`rmse\`, \`null_rmse\`, \`rse\`, ordered by variable and
RSE. Extra columns named \`pct_miss\` are kept.

## Null baseline

By default the baseline value is the mean (or median) of the held-out
\`truth\` values of that variable (every row of \`results_df\` for the
variable counts once), one value per variable shared by all methods.
This is an \*\*oracle\*\* baseline: it uses the very values being
predicted, so it is the best constant for the mean (it minimises the
RMSE) and a method that imputes the training mean scores an RSE slightly
above 1. (The median minimises the absolute error, not the RMSE.) Note
that \[tune_impute_numeric()\] defines its RSE differently, with the
mean of the unmasked training values as the baseline, so its \`mean\`
method scores exactly 1. To use the training data's mean or median here
as well, pass \`baseline\`: a named numeric vector (names are variables)
or a data frame with \`variable\` and \`value\`. If the null RMSE is 0
(all truths equal the baseline) the RSE is \`NA\`. RSEs of different
methods are only comparable when they were scored on the same cells; the
function warns when the cell counts differ.

## See also

\[tbl_best_imputation_method()\], \[plot_rse_by_column()\],
\[plot_rse_vs_missingness()\], \[build_best_imputed_data()\],
\[tune_impute_numeric()\]

## Examples

``` r
set.seed(1)
cells <- data.frame(
  variable = "x", truth = rnorm(40, 10, 2)
)
res <- rbind(
  transform(cells, method = "mean", imputed = 10),
  transform(cells, method = "good", imputed = truth + rnorm(40, 0, 0.5))
)
compare_imputation_rse(res)
#> # A tibble: 2 × 7
#>   variable method     n n_dropped  rmse null_rmse    rse
#>   <chr>    <chr>  <int>     <int> <dbl>     <dbl>  <dbl>
#> 1 x        good      40         0 0.461      1.75 0.0693
#> 2 x        mean      40         0 1.76       1.75 1.01  
```
