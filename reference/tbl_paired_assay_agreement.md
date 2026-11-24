# Paired Assay Agreement Table (Bias, Limits of Agreement, Lin's CCC)

For each row of \`pairs_df\`, compares two measurement columns of the
same specimens: the mean difference (bias) \`method_1 - method_2\`, the
SD of the differences, Bland-Altman 95 percent limits of agreement
(\`bias +/- 1.96 SD\`), Lin's concordance correlation coefficient and
the percentage of rows missing each method.

## Usage

``` r
tbl_paired_assay_agreement(
  data,
  pairs_df,
  id_col = NULL,
  time_col = NULL,
  metrics = c("bias", "sd_diff", "loa_lower", "loa_upper", "ccc", "pct_miss_1",
    "pct_miss_2"),
  digits = 2
)
```

## Arguments

- data:

  A data frame.

- pairs_df:

  Data frame with columns \`parameter\`, \`method_1\` and \`method_2\`
  (names of numeric columns of \`data\`).

- id_col:

  Optional subject column (adds \`n_subjects\`).

- time_col:

  Optional time column; rows with a missing time are excluded, as they
  cannot be matched to a time point.

- metrics:

  Statistics to return, any of \`"bias"\`, \`"sd_diff"\`,
  \`"loa_lower"\`, \`"loa_upper"\`, \`"ccc"\`, \`"pct_miss_1"\`,
  \`"pct_miss_2"\`. \`parameter\`, \`method_1\`, \`method_2\` and
  \`n_pairs\` are always returned.

- digits:

  Decimals for rounding; \`NULL\` for none.

## Value

A tibble with one row per pair. Statistics that cannot be computed
(fewer than two pairs, constant data for the CCC) are \`NA\`.

## Details

Only rows where both methods are present are paired. Repeated
measurements of the same subject are treated as independent, so the
limits are naive for longitudinal data; \`id_col\` only adds the number
of subjects.

## See also

\[tbl_two_sample_compare()\]

## Examples

``` r
d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1.1, 2.3, 2.9, 4.2, 5.4))
tbl_paired_assay_agreement(d, data.frame(parameter = "x", method_1 = "a", method_2 = "b"))
#> # A tibble: 1 × 11
#>   parameter method_1 method_2 n_pairs  bias sd_diff loa_lower loa_upper   ccc
#>   <chr>     <chr>    <chr>      <int> <dbl>   <dbl>     <dbl>     <dbl> <dbl>
#> 1 x         a        b              5 -0.18    0.19     -0.56       0.2  0.99
#> # ℹ 2 more variables: pct_miss_1 <dbl>, pct_miss_2 <dbl>
```
