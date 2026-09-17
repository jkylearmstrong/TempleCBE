# Standard 4-Quadrant Clinical Contingency Report

Generates a standard 4-quadrant clinical report for a 2x2 contingency
table:


    q1 | q2
    q3 | q4
    p = pformat

where q1 is (Row 1, Col 1), q2 is (Row 1, Col 2), q3 is (Row 2, Col 1),
and q4 is (Row 2, Col 2). If any cell count is zero, hypothesis testing
automatically defaults to the mid-p version of Central Fisher's exact
test via
[`cbe_exact2x2`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_exact2x2.md).

## Usage

``` r
cbe_four_quadrant_report(
  data,
  var1 = NULL,
  var2 = NULL,
  label1 = NULL,
  label2 = NULL,
  test = c("auto", "exact", "chisq", "fisher"),
  correct = FALSE,
  p_format = NULL,
  ...
)
```

## Arguments

- data:

  A data frame or a 2x2 matrix/table.

- var1:

  Character string of row variable column name (if `data` is a data
  frame).

- var2:

  Character string of column variable column name (if `data` is a data
  frame).

- label1:

  Display label for row variable.

- label2:

  Display label for column variable.

- test:

  Hypothesis test engine: `"auto"` (default CBE hierarchy: mid-p exact
  if zero-cell, otherwise Central Fisher's exact), `"exact"` (force
  exact test), `"chisq"` (force Pearson's Chi-squared test via
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)), or
  `"fisher"` (force
  [`fisher.test`](https://rdrr.io/r/stats/fisher.test.html)).

- correct:

  Logical; whether to apply continuity correction for `"chisq"` (default
  `FALSE`).

- p_format:

  Function for p-value formatting (default
  [`pformat`](https://jkylearmstrong.github.io/TempleCBE/reference/pformat.md)).

- ...:

  Additional arguments passed to
  [`cbe_contingency_plot`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_contingency_plot.md).

## Value

A list containing:

- quadrants:

  A tibble with counts and percentages for q1, q2, q3, and q4.

- p_value:

  Numeric exact test p-value.

- p_formatted:

  Formatted p-value string (e.g. `"p = 0.024"`).

- test_method:

  Name of the exact test used.

- compact_report:

  Compact 3-line string formatted as
  `q1 | q2 // q3 | q4 // p = pformat`.

- text_card:

  Console-ready formatted 4-quadrant text card.

- plot:

  A `ggplot` object rendering the 4-quadrant square report.

## Examples

``` r
tab <- matrix(c(14, 6, 4, 16), nrow = 2,
              dimnames = list(c("Yes", "No"), c("Active", "Placebo")))
rep <- cbe_four_quadrant_report(tab, label1 = "Response", label2 = "Treatment")
cat(rep$compact_report, "\n\n")
#> q1: n = 14 (35.0%) | q2: n = 4 (10.0%)
#> q3: n = 6 (15.0%) | q4: n = 16 (40.0%)
#> p = 0.004 
#> 
cat(rep$text_card)
#> q1: Yes / Active | q2: Yes / Placebo
#>   n = 14 (35.0%) |   n = 4 (10.0%)
#> -----------------------+-----------------------
#> q3: No / Active | q4: No / Placebo
#>   n = 6 (15.0%) |   n = 16 (40.0%)
#> p = 0.004 (Central Fisher's Exact Test)

# Force Pearson Chi-squared test:
rep_chi <- cbe_four_quadrant_report(tab, test = "chisq")
cat(rep_chi$compact_report, "\n")
#> q1: n = 14 (35.0%) | q2: n = 4 (10.0%)
#> q3: n = 6 (15.0%) | q4: n = 16 (40.0%)
#> p = 0.001 
```
