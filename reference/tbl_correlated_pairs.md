# Table of Highly Correlated Feature Pairs

Lists every pair of numeric features whose absolute correlation is at
least \`cutoff\`, with optional labels and an executive caption ("X of
the Y features (Z correlated"). Where \[drop_correlated_columns()\] says
which columns to remove, this table is for the reader of the report.

## Usage

``` r
tbl_correlated_pairs(
  data,
  features = NULL,
  cutoff = 0.65,
  method = c("pearson", "spearman"),
  metadata = NULL,
  format = c("kable", "gt", "tibble"),
  caption = TRUE,
  digits = 3
)
```

## Arguments

- data:

  A data frame.

- features:

  Numeric columns to check; \`NULL\` uses all numeric columns.

- cutoff:

  Absolute correlation threshold in (0, 1\]; a pair at exactly the
  cutoff counts.

- method:

  \`"pearson"\` or \`"spearman"\` (pairwise complete observations).
  Pairs with an undefined correlation (a constant column, fewer than 3
  shared rows) are not listed.

- metadata:

  Optional data frame with columns \`columns\` (feature names) and
  \`labels\`. Features without a label keep their name.

- format:

  \`"kable"\` (needs \`knitr\`), \`"gt"\` (needs \`gt\`) or
  \`"tibble"\`.

- caption:

  Build the caption (shown as the table caption for \`"kable"\` and
  \`"gt"\`, and always returned as the \`caption\` attribute of the
  tibble form).

- digits:

  Decimals of the correlation in the formatted tables.

## Value

For \`"tibble"\`, a tibble with \`feature_1\`, \`feature_2\`,
\`label_1\`, \`label_2\` and \`correlation\`, sorted by decreasing
absolute correlation, with attributes \`caption\`, \`n_features\` and
\`n_involved\`. For the other formats the formatted table.

## See also

\[drop_correlated_columns()\], \[plot_target_correlation_heatmap()\]

## Examples

``` r
tbl_correlated_pairs(mtcars, cutoff = 0.85, format = "tibble")
#> # A tibble: 4 × 5
#>   feature_1 feature_2 label_1 label_2 correlation
#>   <chr>     <chr>     <chr>   <chr>         <dbl>
#> 1 cyl       disp      cyl     disp          0.902
#> 2 disp      wt        disp    wt            0.888
#> 3 mpg       wt        mpg     wt           -0.868
#> 4 mpg       cyl       mpg     cyl          -0.852
```
