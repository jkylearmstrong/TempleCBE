# Tune Over \`alpha\` for Every Split of a Resample

Calls \[tune_over_alpha()\] on each split of \`object\` and row-binds
the successful fits. Fits that errored are dropped. A split draws its
own random \`alpha\` values, reproducibly: the map runs with
\`furrr_options(seed = TRUE)\`.

## Usage

``` r
summarize_tune_results(
  object,
  ...,
  num_alpha_values = 10,
  num_fixed = 6,
  alphas = NULL,
  formulas = NULL,
  progress = FALSE
)
```

## Arguments

- object:

  A resampling object with a \`splits\` column (e.g. one element of the
  \`inner_resamples\` column of \[rsample::nested_cv()\]).

- ...:

  Arguments passed to \[glmnet_IBS()\] (\`recipe\`, \`feature_names\`,
  \`time_data\`, \`id_col\`, \`cox.ties\`, ...).

- num_alpha_values:

  Total number of \`alpha\` values in the grid.

- num_fixed:

  Number of evenly spaced values from 0 to 1 in the grid.

- alphas:

  Optional numeric vector of \`alpha\` values to use instead of the
  generated grid; in formula mode, one per formula.

- formulas:

  Optional character vector of \`+\`-separated feature sets, or a list
  of one-sided formulas, each fit as a separate model (see
  \[glmnet_IBS()\]'s \`formula\`).

- progress:

  Show a progress bar.

## Value

A tibble of \[glmnet_IBS()\] results with an \`inner_resamples_splits\`
column giving the split's position in \`object\$splits\`.

## See also

\[glmnet_IBS()\], \[tune_over_alpha()\], \[nested_cv_coxnet()\]
