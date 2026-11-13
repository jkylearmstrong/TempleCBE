# Publication-Ready 2x2 Contingency Table with Tests and Risk Measures

Cross-tabulates two binary variables as \`n (percent)\` cells with
marginal totals, an exact and/or chi-squared test, the odds ratio and
relative risk with confidence intervals, and the standard table
footnote. For the institutional layout with a conditional-MLE odds ratio
and mid-p handling use \[table_two_by_two()\].

## Usage

``` r
tbl_contingency_2x2(
  data,
  row_var,
  col_var,
  percent = c("row", "col", "total"),
  test = c("fisher", "chisq", "both"),
  include_risk = TRUE,
  include_total = TRUE,
  digits = 1,
  p_digits = 3,
  note = TRUE,
  conf.level = 0.95
)
```

## Arguments

- data:

  A data frame.

- row_var, col_var:

  Names of the row and column variables. Each must have exactly two
  observed levels after rows with a missing value in either are dropped.

- percent:

  Denominator of the cell percentages: \`"row"\`, \`"col"\` or
  \`"total"\`.

- test:

  \`"fisher"\`, \`"chisq"\` (Pearson, no continuity correction) or
  \`"both"\`.

- include_risk:

  Add the odds ratio and relative risk.

- include_total:

  Add the marginal totals.

- digits:

  Decimals of the percentages.

- p_digits:

  Decimals of the p-values (smaller values print as \`\<0.001\`).

- note:

  Build the footnote text.

- conf.level:

  Confidence level of the risk intervals.

## Value

A list with \`table\` (tibble of formatted cells), \`counts\` (the 2x2
\`table\`), \`tests\` (tibble: \`test\`, \`p_value\`, \`p_formatted\`),
\`risk\` (tibble: \`measure\`, \`estimate\`, \`lower\`, \`upper\`;
\`NULL\` if \`include_risk = FALSE\`), \`note\` (string or \`NULL\`) and
\`n\` (rows used).

## Risk measures

The first level of \`row_var\` is the exposed group and the first level
of \`col_var\` is the outcome. The odds ratio is \`(a \* d) / (b \* c)\`
and the relative risk is \`(a / (a + b)) / (c / (c + d))\`, with Wald
(log-scale) intervals. If any cell is zero, 0.5 is added to every cell
for these two measures only (the counts and percentages are unchanged).
Factor levels set the order; otherwise levels are sorted.

## See also

\[table_two_by_two()\], \[tbl_table_one()\]

## Examples

``` r
res <- tbl_contingency_2x2(mtcars, "am", "vs", test = "both")
res$table
#> # A tibble: 3 × 4
#>   am    `0`        `1`        Total      
#>   <chr> <chr>      <chr>      <chr>      
#> 1 0     12 (63.2%) 7 (36.8%)  19 (100.0%)
#> 2 1     6 (46.2%)  7 (53.8%)  13 (100.0%)
#> 3 Total 18 (56.2%) 14 (43.8%) 32 (100.0%)
res$risk
#> # A tibble: 2 × 4
#>   measure       estimate lower upper
#>   <chr>            <dbl> <dbl> <dbl>
#> 1 Odds ratio        2    0.476  8.40
#> 2 Relative risk     1.37 0.693  2.70
res$note
#> [1] "n = 32. Cells are n (row %). Fisher's exact test, p = 0.473; Pearson's chi-squared test, p = 0.341."
```
