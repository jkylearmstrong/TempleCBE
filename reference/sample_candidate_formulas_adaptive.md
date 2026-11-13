# Sample Candidate Formulas Conditioned on Earlier Tuning Results

The second stage of a two-stage formula search. Stage one is
\[sample_candidate_formulas()\] scored by a tuning run; this function
learns from that run and draws new formulas: 1. the best models are kept
(\`top_quantile\` of the rows by \`metric\`); 2. the number of terms of
those models gives the size range of the new formulas (the 25th
percentile to the median of the winners' sizes); 3. terms that appear
more often among the winners are drawn more often (their share among the
winners, plus a small floor so that no term is shut out); 4. optionally,
terms whose effect points the wrong way are dropped (see \`effects\`).

## Usage

``` r
sample_candidate_formulas_adaptive(
  prior_results_df,
  metric = "IBS",
  lower_is_better = TRUE,
  top_quantile = 0.25,
  filter_direction = TRUE,
  effects = NULL,
  effect_scale = c("ratio", "coef"),
  n_formulas = 5L,
  formula_col = "formula",
  response = NULL,
  required = character(),
  seed = NULL,
  env = parent.frame(),
  data = NULL
)
```

## Arguments

- prior_results_df:

  A data frame of earlier results with a formula column (text or formula
  objects) and a \`metric\` column, such as the output of
  \[as_tuning_results()\].

- metric:

  Name of the performance column (default \`"IBS"\`).

- lower_is_better:

  \`TRUE\` (default; IBS, Brier, AIC) or \`FALSE\` (concordance, AUC).

- top_quantile:

  Share of rows kept as winners, between 0 and 1 (default 0.25).

- filter_direction:

  If \`TRUE\` (default) and \`effects\` is given, terms with an
  unfavourable effect are dropped from the pool. Without \`effects\`
  there is nothing to filter on and this is ignored.

- effects:

  Optional data frame with columns \`term\` and \`estimate\`: the
  univariate effect of each term (for example from \[cox_single()\] or
  \[glm_single()\]).

- effect_scale:

  \`"ratio"\` (default; hazard or odds ratios, favourable when above 1)
  or \`"coef"\` (coefficients, favourable when above 0).

- n_formulas:

  Number of distinct formulas wanted.

- formula_col:

  Name of the formula column (default \`"formula"\`).

- response:

  Left-hand side as text; by default the one of the first formula.

- required:

  Terms that are in every formula.

- seed:

  Optional integer; see \[sample_candidate_formulas()\].

- env:

  Environment of the formulas.

- data:

  Optional data frame (training rows): winning terms that are
  zero-variance columns of it are dropped from the pool unless
  \`required\`; see \[sample_candidate_formulas()\].

## Value

The tibble of \[sample_candidate_formulas()\]; the pool, size range and
weights used are in the attribute \`"adaptive"\`.

## Details

Term and size choices come from the rows given, so pass results from the
\*\*training\*\* folds only if the new formulas are to be assessed on
held-out data (see the data-boundary note of
\[sample_candidate_formulas()\]).

## See also

\[sample_candidate_formulas()\], \[as_tuning_results()\]

## Examples

``` r
prior <- data.frame(
  formula = c("y ~ a + b", "y ~ a + c", "y ~ a + b + c", "y ~ d", "y ~ d + e", "y ~ e"),
  IBS = c(0.10, 0.11, 0.12, 0.20, 0.22, 0.25)
)
sample_candidate_formulas_adaptive(prior, top_quantile = 0.5, n_formulas = 3, seed = 1)
#> # A tibble: 3 × 4
#>      id formula   terms     n_terms
#>   <int> <list>    <list>      <int>
#> 1     1 <formula> <chr [2]>       2
#> 2     2 <formula> <chr [2]>       2
#> 3     3 <formula> <chr [2]>       2
```
