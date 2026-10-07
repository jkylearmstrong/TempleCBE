# Control Parameters for Racing Survival Workflows

Creates a racing control specification via
[`control_race`](https://finetune.tidymodels.org/reference/control_race.html)
with default settings optimized for clinical survival workflows and
nested cross-validation.

## Usage

``` r
control_race_survival(
  save_pred = TRUE,
  parallel_over = c("everything", "resamples", "across"),
  save_workflow = TRUE,
  burn_in = 3,
  num_ties = 10,
  alpha = 0.05,
  randomize = TRUE,
  ...
)

cbe_control_race(
  save_pred = TRUE,
  parallel_over = c("everything", "resamples", "across"),
  save_workflow = TRUE,
  burn_in = 3,
  num_ties = 10,
  alpha = 0.05,
  randomize = TRUE,
  ...
)
```

## Arguments

- save_pred:

  Logical; whether to save out-of-fold assessment predictions (default
  `TRUE`).

- parallel_over:

  How to parallelize execution: `"everything"` (default), `"resamples"`,
  or `"across"`.

- save_workflow:

  Logical; whether to retain the fitted workflow object in the output
  (default `TRUE`), required by
  [`fit_best`](https://tune.tidymodels.org/reference/fit_best.html).

- burn_in:

  Minimum number of resamples evaluated before candidate configurations
  can be eliminated (default 3).

- num_ties:

  Number of bootstrap samples used to evaluate ties in win-fraction
  racing (default 10).

- alpha:

  Significance level threshold for elimination in ANOVA racing (default
  0.05).

- randomize:

  Logical; whether to randomize the order of resamples (default `TRUE`).

- ...:

  Additional arguments forwarded to
  [`control_race`](https://finetune.tidymodels.org/reference/control_race.html).

## Value

A `control_race` object.

## See also

[`tune_race_survival`](https://jkylearmstrong.github.io/TempleCBE/reference/tune_race_survival.md),
[`control_race`](https://finetune.tidymodels.org/reference/control_race.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("finetune", quietly = TRUE)) {
  ctrl <- control_race_survival()
}
} # }
```
