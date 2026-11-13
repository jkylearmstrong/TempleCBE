# Compute a Composite Clinical or Biomarker Index

Combines multiple numeric variables into a single composite score using
standardized z-scores, first principal component (PCA), or weighted
sums.

## Usage

``` r
compute_composite_index(
  data,
  cols,
  method = c("zscore", "pca", "sum", "min_max"),
  weights = NULL,
  index_name = "composite_index",
  polarities = NULL,
  center = TRUE
)
```

## Arguments

- data:

  A data frame or tibble.

- cols:

  Character vector of numeric column names to include in the index.

- method:

  Index construction method: \`"zscore"\` (default, mean of z-scores),
  \`"pca"\` (first principal component score, scaled so higher values
  indicate greater severity), \`"sum"\` (weighted sum), or \`"min_max"\`
  (each variable rescaled to the range 0 to 1 over its observed values,
  optionally centered, then the weighted sum; a constant variable
  contributes 0).

- weights:

  Optional numeric vector of weights matching \`cols\`. If \`NULL\`,
  equal weights are used.

- index_name:

  Character string naming the output index column (default
  \`"composite_index"\`).

- polarities:

  Optional vector of \`+1\` / \`-1\`, one per column of \`cols\` (in
  that order, or named by column): \`-1\` for a variable whose larger
  values mean a \*lower\* index, so that all components push the index
  in the same direction. Multiplies the weights. Not used by \`method =
  "pca"\`, which chooses its own signs (giving \`polarities\` there is
  an error).

- center:

  For \`method = "min_max"\`: if \`TRUE\` (default) each rescaled
  variable is centered on its mean, so an average subject scores 0. To
  center on a baseline visit instead of all rows, use
  \[compute_longitudinal_composite_index()\].

## Value

The input data frame augmented with the new composite index column.

## See also

\[index_variable_table()\], \[center()\]

## Examples

``` r
df <- data.frame(sbp = c(120, 140, 160), dbp = c(80, 90, 100), bmi = c(22, 28, 35))
compute_composite_index(df, cols = c("sbp", "dbp", "bmi"), method = "zscore")
#>   sbp dbp bmi composite_index
#> 1 120  80  22     -0.99113316
#> 2 140  90  28     -0.01707718
#> 3 160 100  35      1.00821035
```
