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
  object. An \`.rds\` file is read with \[readRDS()\], which can run
  code stored in the file: pass only files that you or a process you
  trust wrote.

- ...:

  Unused.

## Value

A standardized \[tibble::tibble()\] guaranteed to contain \`alpha\` and
\`IBS\` columns.

## Details

Maps historical and tidymodels column synonyms to canonical names:

- Integrated Brier Score: \`ibs\`, \`mean_ibs\`,
  \`integrated_brier_score\`, \`brier_score\` -\> \`IBS\`. The generic
  tidymodels columns \`mean\` and \`.estimate\` are taken as the IBS
  only when a \`.metric\` column says that every remaining row is a
  Brier metric (\`"brier_survival_integrated"\`, \`"brier_survival"\`,
  \`"brier"\` or \`"ibs"\`). Without \`.metric\`, a \`mean\` could be
  the mean of any metric (a concordance, for example), so it is not used
  and the call fails with the list of columns found.

- Elastic net mixing: \`alphas\`, \`mixture\` -\> \`alpha\`. A table
  with none of these gets \`alpha = 1\` (lasso) with a warning, because
  the mixing parameter is then an assumption, not something read from
  the data.

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
