# Stratified Baseline "Table 1"

A manuscript baseline table with an overall column, one column per
stratum and an omnibus test p-value, built on
\[gtsummary::tbl_summary()\] so the result can be styled and exported
like any other \`gtsummary\` table (\`gtsummary::as_flex_table()\`,
\`as_gt()\`, ...). \* categorical variables: \`n ( \* continuous
variables: \`mean (SD)\`, or \`median \[Q1, Q3\]\` for those in
\`non_normal_vars\`; \* p-values: Welch's t-test or Welch's one-way
ANOVA for the mean (SD) variables, Wilcoxon or Kruskal-Wallis for the
median variables, chi-squared or Fisher's exact test for categorical
ones (\`gtsummary\` chooses by the expected counts).

## Usage

``` r
tbl_table_one(
  data,
  strata = NULL,
  vars = NULL,
  non_normal_vars = NULL,
  test = TRUE,
  overall = TRUE,
  digits = 2,
  p_digits = 3,
  missing = "ifany"
)
```

## Arguments

- data:

  A data frame.

- strata:

  Name of the grouping column (at least two levels in use). \`NULL\`
  gives an unstratified table without p-values.

- vars:

  Columns to describe; default all columns except \`strata\`.

- non_normal_vars:

  Continuous variables shown as median \[Q1, Q3\] and tested with a rank
  test.

- test:

  Add the p-value column (default \`TRUE\`; ignored without \`strata\`).

- overall:

  Add the overall column (default \`TRUE\`).

- digits:

  Decimals for continuous statistics.

- p_digits:

  Decimals for p-values.

- missing:

  Passed to \[gtsummary::tbl_summary()\] (\`"ifany"\` default).

## Value

A \`gtsummary\` table.

## See also

\[tbl_two_sample_compare()\], \[summary_table()\]

## Examples

``` r
d <- mtcars
d$cyl <- factor(d$cyl)
tbl_table_one(d, strata = "cyl", vars = c("mpg", "hp", "am"), non_normal_vars = "hp")


  

Characteristic
```
