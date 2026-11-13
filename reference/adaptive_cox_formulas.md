# Adaptive Candidate Formulas from Top Models

Keeps the predictors whose odds (or hazard) ratio is above a threshold,
learns the size and term frequencies of the formulas of the best models,
and draws new candidate formulas from them. A front end to
\[sample_candidate_formulas_adaptive()\] for a table of predictor ratios
and a table of already selected (low-error) models.

## Usage

``` r
adaptive_cox_formulas(
  summary_df,
  low_ib_models,
  n_formulas = 5,
  or_threshold = 1,
  seed = NULL
)
```

## Arguments

- summary_df:

  Data frame with \`predictor\` and \`odds_ratio\` (ratios; use
  \`exp(coef)\` for Cox models). Predictors are matched to the terms of
  the formulas by name.

- low_ib_models:

  Data frame of the best models with a \`formula\` column (text or
  formula objects). All rows count as winners; rank and filter them
  first.

- n_formulas:

  Number of distinct formulas wanted.

- or_threshold:

  Keep predictors with a ratio \*\*above\*\* this value (1 = harmful or
  associated with more events).

- seed:

  Optional integer; see \[sample_candidate_formulas()\].

## Value

The tibble of \[sample_candidate_formulas()\]; details in the attribute
\`"adaptive"\`.

## See also

\[sample_candidate_formulas_adaptive()\], \[sample_weighted_formulas()\]

## Examples

``` r
ratios <- data.frame(predictor = c("a", "b", "c", "d"), odds_ratio = c(1.8, 0.6, 1.2, 2.5))
top <- data.frame(formula = c("y ~ a + c", "y ~ a + d", "y ~ a + c + d"))
adaptive_cox_formulas(ratios, top, n_formulas = 3, seed = 1)
#> # A tibble: 3 × 4
#>      id formula   terms     n_terms
#>   <int> <list>    <list>      <int>
#> 1     1 <formula> <chr [2]>       2
#> 2     2 <formula> <chr [2]>       2
#> 3     3 <formula> <chr [2]>       2
```
