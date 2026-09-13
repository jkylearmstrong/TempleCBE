# Tune a Penalized Cox Model Over a Grid of \`alpha\` Values

Runs \[glmnet_IBS()\] on one resampling split for each value in an
\`alpha\` grid, or for each of a set of candidate formulas, in parallel
via \[furrr::future_map()\]. Set a \[future::plan()\] first to run in
parallel; this function never sets one.

## Usage

``` r
tune_over_alpha(
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

  An \`rsplit\`.

- ...:

  Arguments passed to \[glmnet_IBS()\] (\`recipe\`, \`feature_names\`,
  \`time_data\`, \`id_col\`, \`censoring_weights\`, ...).

- num_alpha_values:

  Total number of \`alpha\` values in the grid.

- num_fixed:

  Number of evenly spaced values from 0 to 1 in the grid.

- alphas:

  Optional numeric vector of \`alpha\` values to use instead of the
  generated grid; in formula mode, one per formula.

- formulas:

  Optional character vector of \`+\`-separated feature sets, each fit as
  a separate model (see \[glmnet_IBS()\]'s \`formula\`).

- progress:

  Show a progress bar.

## Value

A list named by \`alpha\`, one element per fit, each the output of
\[purrr::safely()\]: a list with \`result\` (the \[glmnet_IBS()\]
tibble, or \`NULL\`) and \`error\` (\`NULL\`, or the condition).

## Details

\*\*Grid mode\*\* (the default). The grid is \`num_fixed\` evenly spaced
values from 0 to 1, plus \`num_alpha_values - num_fixed\` values drawn
uniformly at random, one per gap between consecutive fixed values
(cycling through the gaps if there are more random draws than gaps).

\*\*Formula mode\*\* (\`formulas\` given). One fit per formula, each
with its own \`alpha\`: the matching element of \`alphas\`, or a value
drawn uniformly from 0 to 1 when \`alphas\` is \`NULL\`. Each result
gains a \`formula\` column.

Random draws use the R session's random number stream, so call
\[set.seed()\] first for reproducible values. Model fits run with
\`furrr_options(seed = TRUE)\`, so they are reproducible too.

## See also

\[glmnet_IBS()\], \[summarize_tune_results()\]
