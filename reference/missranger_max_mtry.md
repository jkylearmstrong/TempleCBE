# Largest \`mtry\` \`missRanger\` Will Accept For a Data Set

\`missRanger\` admits a \*\*smaller\*\* \`mtry\` than \`missForest\` on
the same data, and the difference is easy to trip over: exceeding it
surfaces as \`ranger\`'s catch-all \`"User interrupt or internal
error."\`, which names neither \`mtry\` nor the real bound.

## Usage

``` r
missranger_max_mtry(data)
```

## Arguments

- data:

  A data frame or tibble, after any identifier columns have been
  excluded.

## Value

A single integer: the largest admissible \`mtry\`, at least \`1\`.

## Details

\`missForest\` predicts every column from every other one, so its bound
is simply \`ncol(data) - 1\`. \`missRanger\` builds its predictor pool
up over the first iteration instead. It starts with \`completed\`, the
set of \*\*fully-observed\*\* columns, imputes the targets in increasing
order of missingness, and adds each one to \`completed\` as it goes. The
first target therefore sees only the complete columns, and \`mtry\` must
fit \*that\* pool:

\* With \`k\` usable complete columns, the bound is \`k\`. \* With none,
the first target falls back to univariate imputation and the second sees
a single predictor, so the bound is \`1\` – no matter how wide the data
is.

Constant columns are excluded from the count, matching \`missRanger\`'s
own handling of them as features.

The practical consequence: a sweep is only informative when some columns
are complete. That is the usual shape of the intended use – imputing a
domain's incomplete outputs against its fully-observed inputs – but a
frame in which \*every\* column has missing values admits \`mtry = 1\`
alone, and there is nothing to sweep.

## See also

\[missranger_sweep_mtry()\]

## Examples

``` r
# Two complete columns -> mtry may be 1 or 2.
missranger_max_mtry(data.frame(a = c(1, NA, 3), b = c(1, 2, 3), c = c(4, 5, 6)))
#> [1] 2

# Every column has a gap -> only mtry = 1 is admissible.
missranger_max_mtry(data.frame(a = c(1, NA, 3), b = c(NA, 2, 3), c = c(4, 5, NA)))
#> [1] 1
```
