# Run \`missForest\` at a Single \`mtry\` and Report Variablewise OOB Error

Fits \[missForest::missForest()\] once at a given \`mtry\`, returning
both the completed data and a tidy, per-column out-of-bag error table.
This is the unit of work swept over by \[missforest_sweep_mtry()\].

## Usage

``` r
missforest_oob_by_mtry(data, mtry, ntree = 100, maxiter = 10)
```

## Arguments

- data:

  A data frame or tibble containing the columns to impute. Any
  identifier or time columns should already be removed; see
  \[missforest_sweep_mtry()\]'s \`exclude\` argument. Columns that are
  entirely \`NA\` are refused – see Details.

- mtry:

  Number of variables randomly sampled as candidates at each split. A
  single positive integer.

- ntree:

  Number of trees per forest. Passed to \[missForest::missForest()\].
  Default \`100\`.

- maxiter:

  Maximum number of imputation iterations. Passed to
  \[missForest::missForest()\]. Default \`10\`.

## Value

A list of two elements:

- \`ximp\`:

  A tibble of the completed (imputed) data.

- \`oob_error\`:

  A tibble with one row per column of \`data\`, and columns \`column\`,
  \`error_type\` (\`"MSE"\` for numeric columns, \`"PFC"\` for factors),
  \`error\`, and \`mtry\`.

## Details

\`missForest\` requires every column to be numeric or a factor, so
character columns are converted to factors here. The conversion is
idempotent – a caller that has already converted its own characters sees
no change – which is what lets this single function replace call sites
that previously converted at different points.

Columns that are entirely \`NA\` are refused with an error.
\[missForest::missForest()\] silently drops them, which both breaks the
per-column error table this function returns and would quietly shrink
the predictor set that swept \`mtry\` values index into. Such columns
arise easily – a mapping-driven reader that fills absent source columns
with a bare \`NA\` produces full-length logical columns that pass
name-based schema checks. Drop or fill them before imputing.

## Reproducibility

\`missForest\` is stochastic. This function does not set a seed –
seeding is the caller's responsibility, and \[missforest_sweep_mtry()\]
handles it in a parallel-safe way. See that function's Reproducibility
section.

## See also

\[missforest_sweep_mtry()\], \[missforest_impute_by_mtry()\]

## Examples

``` r
# \donttest{
if (requireNamespace("missForest", quietly = TRUE)) {
  set.seed(1)
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c(2, NA, 6, 8, 10))
  res <- missforest_oob_by_mtry(df, mtry = 1)
  res$oob_error
}
#> # A tibble: 2 × 4
#>   column error_type error  mtry
#>   <chr>  <chr>      <dbl> <int>
#> 1 a      MSE         4.61     1
#> 2 b      MSE        14.6      1
# }
```
