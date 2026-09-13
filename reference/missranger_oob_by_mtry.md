# Run \`missRanger\` at a Single \`mtry\` and Report Per-Column OOB Error

Fits \[missRanger::missRanger()\] once at a given \`mtry\`, returning
both the completed data and a tidy, per-column out-of-bag error table.
This is the unit of work swept over by \[missranger_sweep_mtry()\], and
the \`ranger\`-based counterpart to \[missforest_oob_by_mtry()\].

## Usage

``` r
missranger_oob_by_mtry(
  data,
  mtry,
  num.trees = 500,
  pmm.k = 0,
  maxiter = 10,
  seed = NULL,
  num.threads = 1,
  keep_forests = FALSE
)
```

## Arguments

- data:

  A data frame or tibble containing the columns to impute. Any
  identifier or time columns should already be removed; see
  \[missranger_sweep_mtry()\]'s \`exclude\` argument. Columns that are
  entirely \`NA\` are refused – see Details.

- mtry:

  Number of variables randomly sampled as candidates at each split. A
  single positive integer.

- num.trees:

  Number of trees per forest. Passed to \[missRanger::missRanger()\].
  Default \`500\`, that function's own default.

- pmm.k:

  Number of predictive-mean-matching donors. \`0\` (the default)
  disables PMM and imputes with the forest prediction directly.

- maxiter:

  Maximum number of imputation iterations. Default \`10\`.

- seed:

  Integer seed passed to \[missRanger::missRanger()\], or \`NULL\`
  (default) to leave it unseeded. See Reproducibility.

- num.threads:

  Threads used by \`ranger\` \*within\* one fit. Defaults to \`1\`; see
  the Threading section, which explains why the \`ranger\` default is
  the wrong one here.

- keep_forests:

  Whether to retain the fitted forests on the returned object. Default
  \`FALSE\`. See Details.

## Value

A list of two elements, plus a third when \`keep_forests = TRUE\`:

- \`ximp\`:

  A tibble of the completed (imputed) data.

- \`oob_error\`:

  A tibble with columns \`column\`, \`error_type\`, \`error\`, and
  \`mtry\` – one row per \*imputed\* column. See Details.

- \`fit\`:

  The \`missRanger\` object, when \`keep_forests = TRUE\`.

## Details

\*\*Error rows cover imputed columns only.\*\* \`missForest\` run
variablewise reports an OOB error for every column; \`missRanger\` fits
forests only for columns that actually have missing values, so complete
columns get no row. \[missranger_sweep_mtry()\] carries those columns
through unchanged.

\*\*Errors are not comparable across engines.\*\* \`missForest\` reports
raw MSE and PFC. \`missRanger\` reports \`ranger\`'s scaled OOB
prediction error – \`1 - R^2\` for numeric targets and classification
error for categorical ones – where roughly \`1\` means no better than
predicting the mean. Compare \`mtry\` values within one engine; do not
compare an \`error\` column from one against the other.

\*\*All-\`NA\` columns are refused.\*\* \`missRanger\` does not drop
them the way \`missForest\` does; it silently returns them still
entirely \`NA\`, producing a frame that looks imputed but is not. Such
columns arise easily – a mapping-driven reader that fills absent source
columns with a bare \`NA\` yields full-length logical columns that pass
name-based schema checks.

\*\*\`keep_forests\` is the seam for applying to new data.\*\*
\`missForest\` discards its forests, which is why it cannot be applied
to a later batch. \`missRanger\` can retain them, making \`predict()\`
on new data possible. This function only exposes the option; a fit/apply
pair built on it is separate work.

## Threading

\`ranger\` defaults to using every available core. Inside a parallel
\`mtry\` sweep that nests one thread pool inside another and
oversubscribes the machine badly – workers times cores threads competing
for the same cores. \`num.threads\` therefore defaults to \`1\` here,
leaving parallelism to the sweep. Raise it only when running a single
fit on an otherwise idle box.

## Reproducibility

Unlike \`missForest\`, \`missRanger\` takes its own \`seed\` and uses it
to seed the fit directly, so a seeded fit is fully determined by
\`seed\` regardless of ambient RNG state, worker count, or \`future\`
plan. That makes seeded \`missranger\_\*\` sweeps reproducible by
construction rather than by careful stream management. The architecture
caveat still applies: identical seeds do not guarantee bit-identical
results across x86_64 and arm64, because split selection compares
floating-point impurity sums.

## See also

\[missranger_sweep_mtry()\], \[missforest_oob_by_mtry()\]

## Examples

``` r
# \donttest{
if (requireNamespace("missRanger", quietly = TRUE)) {
  df <- data.frame(
    a = c(1, 2, NA, 4, 5, 6, 7, 8),
    b = c(2, NA, 6, 8, 10, 12, 14, 16),
    c = c(5, 4, 3, NA, 1, 2, 3, 4)
  )
  res <- missranger_oob_by_mtry(df, mtry = 1, num.trees = 50, seed = 1)
  res$oob_error
}
#> # A tibble: 3 × 4
#>   column error_type error  mtry
#>   <chr>  <chr>      <dbl> <int>
#> 1 a      1-R2       1         1
#> 2 b      1-R2       0.446     1
#> 3 c      1-R2       1.10      1
# }
```
