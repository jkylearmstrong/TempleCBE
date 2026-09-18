# Institutional Categorical Hypothesis Test for gtsummary and Batch Testing

Standard categorical hypothesis testing engine conforming to the CBE
statistical protocol. Operates as a drop-in custom test for
[`add_p`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)
or as a standalone batch testing engine with optional furrr
parallelization.

## Usage

``` r
cbe_test_categorical(
  data,
  variable = NULL,
  by = NULL,
  test = c("auto", "exact", "chisq", "fisher"),
  correct = FALSE,
  B = 2000L,
  simulate.p.value = NULL,
  parallel = FALSE,
  n_chunks = 4L,
  group = NULL,
  type = NULL,
  test.args = NULL,
  adj.vars = NULL,
  conf.level = NULL,
  tbl = NULL,
  continuous_variable = NULL,
  ...
)
```

## Arguments

- data:

  Data frame supplied directly or by gtsummary.

- variable:

  Character string column name for the feature being tested, a character
  vector of variable names for batch testing, or a two-sided
  [`formula`](https://rdrr.io/r/stats/formula.html) of the form
  `response ~ grouping_var` or `var1 + var2 ~ grouping_var`. If `NULL`,
  all categorical/factor columns in `data` (excluding `by`) are tested.

- by:

  Character string column name for the stratifying/grouping variable.
  Can be omitted when `variable` is a formula.

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

- B:

  Integer; number of Monte Carlo replicates when Fisher's exact test
  with simulation is triggered (default `2000L`).

- simulate.p.value:

  Optional logical; if specified, explicitly enables or disables Monte
  Carlo simulation for Fisher's test. If `NULL` (default), auto-triggers
  simulation when table total exceeds 500 or any dimension exceeds 2.

- parallel:

  Logical; whether to use furrr for parallel execution (for batch
  testing across multiple variables or chunking high-`B` simulations).
  Requires setting a
  [`plan`](https://future.futureverse.org/reference/plan.html)
  beforehand.

- n_chunks:

  Integer; number of chunks to partition `B` into when running parallel
  simulations with `B >= 10000` (default 4L).

- group, type, test.args, adj.vars, conf.level, tbl,
  continuous_variable:

  Unused; accepted and ignored so this function satisfies gtsummary's
  custom-test calling convention (see
  [`tests`](https://www.danieldsjoberg.com/gtsummary/reference/tests.html))
  without those arguments leaking into `...` and reaching
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)/
  [`fisher.test`](https://rdrr.io/r/stats/fisher.test.html), neither of
  which accepts extra named arguments.

- ...:

  Additional arguments passed to
  [`exact2x2`](https://rdrr.io/pkg/exact2x2/man/exact2x2.html) or
  [`fisher.test`](https://rdrr.io/r/stats/fisher.test.html).

## Value

When `variable` is a single string (such as when called by gtsummary),
returns a one-row tibble compliant with gtsummary's custom test
specification (`p.value`, `statistic`, `parameter`, `method`). When
`variable` is a vector or `NULL`, returns a tidy tibble of test results
across all evaluated variables.

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
library(gtsummary)
trial |>
  tbl_summary(by = trt, include = c(response, death, grade)) |>
  add_p(test = all_categorical() ~ cbe_test_categorical) |>
  separate_p_footnotes()


  

Characteristic
```
