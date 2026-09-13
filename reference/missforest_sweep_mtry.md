# Impute a Data Frame by Sweeping \`missForest\` Over \`mtry\`

Runs \[missForest::missForest()\] once for every candidate \`mtry\`,
then keeps, \*\*for each column independently\*\*, the imputation from
whichever \`mtry\` minimised that column's out-of-bag error. Columns in
the same data frame may therefore be imputed at different \`mtry\`
values.

## Usage

``` r
missforest_sweep_mtry(
  data,
  exclude = NULL,
  mtry_values = NULL,
  ntree = 100,
  maxiter = 10,
  seed = TRUE,
  max_pct_missing = NULL,
  parallel = TRUE
)
```

## Arguments

- data:

  A data frame or tibble to impute. After \`exclude\` is applied, any
  remaining column that is entirely \`NA\` is refused with an error; see
  \[missforest_oob_by_mtry()\]'s Details.

- exclude:

  Character vector of columns to hold out of the imputation entirely –
  typically subject identifiers and time columns. They are removed
  before fitting and re-attached, unchanged, to the result. Default
  \`NULL\`.

- mtry_values:

  Integer vector of \`mtry\` values to sweep. Defaults to \`1:(p - 1)\`,
  where \`p\` is the number of columns remaining after \`exclude\`.

- ntree, maxiter:

  Passed to \[missforest_oob_by_mtry()\].

- seed:

  Controls parallel-safe random number generation; passed to
  \[furrr::furrr_options()\]. \`TRUE\` (the default) draws reproducible
  streams from the current RNG state, so \`set.seed()\` beforehand makes
  the whole sweep reproducible. A single integer seeds the sweep
  self-containedly, independent of ambient RNG state. See
  Reproducibility.

- max_pct_missing:

  Optional proportion in \`(0, 1\]\`. Columns missing a greater share
  than this are held out of the imputation and carried through
  unimputed, and are reported in \`excluded_high_missing\`. \`NULL\`
  (the default) imputes every column regardless of how sparse it is.
  Imputing a column observed in a handful of rows manufactures values
  rather than recovering them, and nothing downstream can tell the
  difference; a threshold states that judgement as a rule that carries
  to the next data set, instead of naming the offending column inline.
  Note a threshold below \`1\` subsumes the all-\`NA\` refusal, since an
  all-\`NA\` column exceeds every threshold.

- parallel:

  Whether to evaluate the sweep with \[furrr::future_map()\]. Default
  \`TRUE\`. When \`FALSE\`, runs sequentially via \[purrr::map()\].

## Value

A list of four elements:

- \`imp_data\`:

  A tibble of the imputed data, with \`exclude\` columns re-attached and
  the original column order restored.

- \`oob_error\`:

  A tibble of every column's OOB error at every swept \`mtry\`.

- \`best\`:

  The winning row per column – the subset of \`oob_error\` minimising
  \`error\` within each \`column\`, ties broken by first occurrence.

- \`excluded_high_missing\`:

  Columns held out by \`max_pct_missing\`, carried through unimputed.
  \`character(0)\` when none were.

## Reproducibility

\`missForest\` is stochastic, so an unseeded sweep is not reproducible.
In parallel mode the seed \*\*must\*\* reach \[furrr::future_map()\]'s
\`.options\`; attaching \`furrr_options()\` to \[future::plan()\]
instead is silently ignored (\`future\` warns about an unknown argument
and proceeds unseeded). This function always passes \`seed\` through to
the \`future_map()\` call.

Seeded this way, results are independent of the number of workers and of
the \`future\` plan, so a Windows \`multisession\` run and a Linux
\`multicore\` run draw identical random streams. Note that identical
streams do not by themselves guarantee bit-identical output across
\*architectures\*: tree splits compare floating-point impurity sums, and
differences in FMA contraction or BLAS between x86_64 and arm64 can
resolve a near-tie differently. Verify cross-architecture agreement
rather than assuming it.

The sequential path (\`parallel = FALSE\`) seeds with \[set.seed()\]
when \`seed\` is a number. It is internally reproducible but will not
match the parallel path, which uses L'Ecuyer-CMRG streams.

## Parallel plan

This function never calls \[future::plan()\] – the appropriate backend
differs by machine, and choosing one here would override the caller's.
Set a plan before calling, for example
\`future::plan(future::multisession, workers =
future::availableCores() - 1)\`.

## See also

\[missforest_oob_by_mtry()\], \[missforest_impute_by_mtry()\]

## Examples

``` r
# \donttest{
if (requireNamespace("missForest", quietly = TRUE)) {
  df <- data.frame(
    id = 1:8,
    a = c(1, 2, NA, 4, 5, 6, NA, 8),
    b = c(2, NA, 6, 8, 10, 12, 14, NA),
    c = c(5, 4, 3, NA, 1, 2, 3, 4)
  )
  res <- missforest_sweep_mtry(df, exclude = "id", seed = 42, parallel = FALSE)
  res$best
}
#> # A tibble: 3 × 4
#>   column error_type error  mtry
#>   <chr>  <chr>      <dbl> <int>
#> 1 a      MSE         7.78     1
#> 2 b      MSE        21.6      1
#> 3 c      MSE         2.07     2
# }
```
