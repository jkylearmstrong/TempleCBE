# Institutional Categorical Hypothesis Test for gtsummary

Standard categorical hypothesis testing engine conforming to the CBE
statistical protocol and designed as a drop-in custom test for
[`add_p`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html).

## Usage

``` r
cbe_test_categorical(
  data,
  variable,
  by,
  test = c("auto", "exact", "chisq", "fisher"),
  correct = FALSE,
  ...
)
```

## Arguments

- data:

  Data frame supplied by gtsummary.

- variable:

  Character string column name for the feature being tested.

- by:

  Character string column name for the stratifying/grouping variable.

- test:

  Hypothesis test engine: `"auto"` (default CBE hierarchy), `"exact"`
  (force Central Fisher exact test for 2x2 or simulated Fisher for RxC),
  `"chisq"` (force Pearson's Chi-squared test via
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)), or
  `"fisher"` (force
  [`fisher.test`](https://rdrr.io/r/stats/fisher.test.html)).

- correct:

  Logical; whether to apply continuity correction when `test = "chisq"`
  (default `FALSE` following CBE standard protocol).

- ...:

  Additional arguments (ignored or passed through).

## Value

A tibble with `p.value` and descriptive `method` compliant with
gtsummary's custom test requirements.

## Details

The function follows a rigorous 4-rule hierarchy:

1.  **2x2 table with zero cell**: Uses Central Fisher's exact test with
    mid-p adjustment
    ([`exact2x2`](https://rdrr.io/pkg/exact2x2/man/exact2x2.html) with
    `midp = TRUE`) to avoid extreme conservatism.

2.  **2x2 table without zero cell**: Uses Central Fisher's exact test
    ([`exact2x2`](https://rdrr.io/pkg/exact2x2/man/exact2x2.html) with
    `midp = FALSE`).

3.  **RxC table with sparse counts**: If any expected cell count is less
    than 5, runs Fisher's exact test with Monte Carlo simulation
    ([`fisher.test`](https://rdrr.io/r/stats/fisher.test.html) with
    `simulate.p.value = TRUE`).

4.  **RxC table with adequate counts**: If all expected cell counts are
    at least 5, runs Pearson's Chi-squared test without continuity
    correction ([`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)
    with `correct = FALSE`).

## Examples

``` r
if (FALSE) { # \dontrun{
library(gtsummary)
trial |>
  tbl_summary(by = trt, include = c(response, death, grade)) |>
  add_p(test = all_categorical() ~ cbe_test_categorical) |>
  separate_p_footnotes()
} # }
```
