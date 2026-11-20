# Candidate Formulas Weighted by Feature Importance

Draws random subsets of features, each feature chosen with probability
related to its weight (for example the absolute log hazard ratio of a
univariate screen), as candidate model formulas. A thin front end to
\[sample_candidate_formulas()\], which has more constraints (required
terms, exclusions, hierarchy).

## Usage

``` r
sample_weighted_formulas(
  feature_weights,
  n_samples = 25,
  min_terms = 2,
  max_terms = length(feature_weights),
  include_full = TRUE,
  response = NULL,
  seed = NULL,
  data = NULL
)
```

## Arguments

- feature_weights:

  Named numeric vector of non-negative weights (names = features). A
  feature with weight 0 is never drawn, except in the full formula.

- n_samples:

  Number of distinct formulas.

- min_terms, max_terms:

  Smallest and largest number of features per formula.

- include_full:

  Also include the formula with every feature (added if the draw did not
  produce it, so the result can have \`n_samples + 1\` rows).

- response:

  Optional left-hand side as text, for example \`"Surv(time, status)"\`.

- seed:

  Optional integer; the caller's random-number state is restored.

- data:

  Optional data frame (training rows). Features that are zero-variance
  columns of it are removed before sampling (also from the full
  formula), with a message.

## Value

The tibble of \[sample_candidate_formulas()\] (\`id\`, \`formula\`,
\`terms\`, \`n_terms\`).

## Details

Compute weights from the \*\*training\*\* data of the current fold only;
weights from all rows leak held-out information into the candidate set.

## See also

\[adaptive_cox_formulas()\], \[expand_feature_formulas()\],
\[calculate_feature_weights()\]

## Examples

``` r
w <- c(age = 0.8, bmi = 0.2, lactate = 1.1, hr = 0.5)
sample_weighted_formulas(w, n_samples = 4, min_terms = 2, max_terms = 3, seed = 1)
#> # A tibble: 5 × 4
#>      id formula   terms     n_terms
#>   <int> <list>    <list>      <int>
#> 1     1 <formula> <chr [2]>       2
#> 2     2 <formula> <chr [2]>       2
#> 3     3 <formula> <chr [2]>       2
#> 4     4 <formula> <chr [3]>       3
#> 5     5 <formula> <chr [4]>       4
```
