# Coerce Historical or Tidymodels Tuning Outputs to Standardized Schema (G5)

Adapts older legacy tuning outputs (\`tuning_results_df\_\*.rds\`),
tidymodels tuning results, or generic hyperparameter evaluation tables
to the standardized schema required by \[compare_tuning_runs()\],
\[select_candidate_models()\], and \[plot.cbe_nested_survival_run()\].

## Usage

``` r
as_tuning_results(x, ...)
```

## Arguments

- x:

  A data frame, tibble, path to an \`.rds\` file, or a tuning result
  object.

- ...:

  Unused.

## Value

A standardized \[tibble::tibble()\] guaranteed to contain \`alpha\` and
\`IBS\` columns.

## Details

Maps historical and tidymodels column synonyms to canonical names:

- Integrated Brier Score: \`ibs\`, \`mean_ibs\`, \`mean\` (when
  \`.metric\` is \`"brier_survival_integrated"\`), \`brier_score\` -\>
  \`IBS\`.

- Elastic net mixing: \`alphas\`, \`mixture\` -\> \`alpha\`.

- Penalty parameter: \`penalty\`, \`lambdas\` -\> \`lambda\`.

- Model formula: \`formulas\`, \`formula_str\` -\> \`formula\`.

- Resampling split: \`split\`, \`splits\`, \`resample\`, \`id\` -\>
  \`inner_resamples_splits\`.

- Outer cross-validation fold: \`fold\`, \`outer_id\` -\>
  \`outer_fold\`.

## See also

\[compare_tuning_runs()\], \[select_candidate_models()\]

## Examples

``` r
# Historical legacy format with lowercase column names
old_df <- data.frame(alphas = c(0.1, 0.5), ibs = c(0.12, 0.10), penalty = c(0.01, 0.02))
standardized <- as_tuning_results(old_df)
names(standardized)
#> [1] "alphas"    "ibs"       "penalty"   "IBS"       "alpha"     "lambda"   
#> [7] ".metric"   ".estimate"
```
