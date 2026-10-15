# Select Candidate Models From Tuning Results

Takes results from \[summarize_tune_results()\] or
\[tune_over_alpha()\], identifies the best-performing \`alpha\` values
(e.g. within the top \`ntile\` of integrated Brier scores), and crosses
them with candidate formulas to construct an outer evaluation grid.

## Usage

``` r
select_candidate_models(
  tune_results,
  formulas,
  ntile = 0.25,
  top_n = NULL,
  by = c("row", "alpha")
)
```

## Arguments

- tune_results:

  A data frame containing tuning results with at least \`alpha\` and
  \`IBS\` columns (e.g. from \[summarize_tune_results()\]).

- formulas:

  Character vector of formulas, list of formula objects, or a single
  formula to evaluate.

- ntile:

  Numeric quantile cutoff (between 0 and 1, default \`0.25\`) for
  selecting the best alphas with lowest IBS.

- top_n:

  Optional integer number of best alphas to keep (overrides \`ntile\` if
  provided).

- by:

  Selection granularity: \`"row"\` (default, selects alphas appearing in
  the top \`ntile\` of individual tune result rows, e.g. \`ntile(IBS, 5)
  == 1\`) or \`"alpha"\` (summarizes mean IBS across all rows per alpha
  before selecting).

## Value

A tibble with columns \`model\`, \`alpha\`, and \`formula\`.

## See also

\[score_outer()\], \[compare_censoring_weights()\],
\[summarize_tune_results()\]
