# Two-Group Comparison Table for Continuous Variables

One row per variable comparing two groups, ready for a manuscript: group
sizes, mean (SD) in each group, the difference with its confidence
interval, and a p-value.

## Usage

``` r
tbl_two_sample_compare(
  data,
  vars,
  group_col,
  test = c("welch", "student", "wilcoxon"),
  conf_level = 0.95,
  digits = 2,
  p_digits = 3
)
```

## Arguments

- data:

  A data frame.

- vars:

  Character vector of numeric columns.

- group_col:

  Name of the grouping column; it must have exactly two non-missing
  values.

- test:

  \`"welch"\`, \`"student"\` or \`"wilcoxon"\`.

- conf_level:

  Confidence level for the difference (default 0.95).

- digits:

  Decimals for means, SDs and the difference.

- p_digits:

  Decimals for the p-value (values below the last digit print as
  \`\<0.001\`).

## Value

A tibble with \`Variable\`, \`N (\<group 1\>)\`, \`Mean (SD) \<group
1\>\`, \`N (\<group 2\>)\`, \`Mean (SD) \<group 2\>\`, \`Difference
\[95% CI\]\` and \`p-value\` (all text), and attributes \`groups\` (the
two levels), \`test\` and \`numeric\` (the unformatted results as a
tibble).

## Difference and tests

The difference is \*\*first level minus second level\*\* of
\`group_col\` (the factor order, or sorted values if it is not a
factor). \`"welch"\` (default) is Welch's t-test (unequal variances) and
\`"student"\` the pooled-variance t-test; both give the mean difference
and its confidence interval. \`"wilcoxon"\` is the Wilcoxon rank-sum
test; its difference is the Hodges-Lehmann estimate (a shift in
location, not a difference in means) with its confidence interval. The
p-value is kept when the estimate or interval cannot be computed (those
are then blank), and it is computed even when both groups are constant
at different values. Each group's \`N\` counts the non-missing values of
that variable in that group. A variable with fewer than 2 values in a
group, or constant in both, gets \`NA\` for the test.

## See also

\[table_two_by_two()\], \[compare_domain_datasets()\]

## Examples

``` r
tbl_two_sample_compare(mtcars, c("mpg", "hp", "wt"), "am")
#> # A tibble: 3 × 7
#>   Variable `N (0)` `Mean (SD) 0`  `N (1)` `Mean (SD) 1`  `Difference [95% CI]`
#>   <chr>    <chr>   <chr>          <chr>   <chr>          <chr>                
#> 1 mpg      19      17.15 (3.83)   13      24.39 (6.17)   -7.24 [-11.28, -3.21]
#> 2 hp       19      160.26 (53.91) 13      126.85 (84.06) 33.42 [-21.88, 88.71]
#> 3 wt       19      3.77 (0.78)    13      2.41 (0.62)    1.36 [0.85, 1.86]    
#> # ℹ 1 more variable: `p-value` <chr>
```
