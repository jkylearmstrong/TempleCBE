# Tune Imputation for a Whole Data Set

Extends \[tune_impute_workflows()\] — which benchmarks recipes and
models for predicting \*\*one\*\* numeric variable — to a whole data
set, so the user does not have to loop it per variable. With \`engine =
"workflows"\` (default), each incomplete numeric column is tuned in turn
with \[tune_impute_workflows()\] (same \`recipes\`, \`models\`,
\`folds\`, \`grid\` and \`race\`, with the seed advanced by one per
variable so each variable's folds are reproducible and distinct), and
the tuned imputations are combined into one completed data set. With
\`engine = "missforest"\` or \`"missranger"\`, the whole data set is
instead imputed by sweeping \`mtry\` with \[missforest_sweep_mtry()\] /
\[missranger_sweep_mtry()\], which already operate on full data but only
tune \`mtry\`; extra arguments (\`mtry_values\`, \`ntree\` /
\`num.trees\`, \`parallel\`, \`max_pct_missing\`, \`checkpoint_dir\`,
\`shard\`, ...) are passed through to the sweep.

## Usage

``` r
tune_impute_dataset(
  data,
  targets = NULL,
  engine = c("workflows", "missforest", "missranger"),
  exclude = NULL,
  recipes = NULL,
  models = NULL,
  folds = 5,
  grid = 10,
  race = TRUE,
  seed = 123,
  verbose = FALSE,
  ...
)

# S3 method for class 'impute_tune_dataset'
print(x, ...)
```

## Arguments

- data:

  A data frame.

- targets:

  Character vector of columns to impute, or \`NULL\` (default) for every
  incomplete numeric column ( \`"workflows"\` engine) or every
  incomplete column (\`"missforest"\` / \`"missranger"\` engines). For
  the forest engines \`targets\` only selects which columns are
  summarised in \`\$summary\`: the sweep imputes the whole data set, so
  non-target incomplete columns are filled in \`\$data\` as well (see
  the Forest engines section).

- engine:

  \`"workflows"\`, \`"missforest"\` or \`"missranger"\`.

- exclude:

  Character vector of columns to hold out of the imputation entirely
  (identifiers, time columns). Passed through to the sweep for the
  forest engines; for the \`"workflows"\` engine the columns are simply
  never targets and are never used as predictors either (they are
  dropped from the tuning data).

- recipes, models, folds, grid, race, seed, verbose:

  Passed to \[tune_impute_workflows()\] for the \`"workflows"\` engine.

- ...:

  For the forest engines, passed to \[missforest_sweep_mtry()\] /
  \[missranger_sweep_mtry()\] (for example \`mtry_values\`, \`ntree\`,
  \`num.trees\`, \`parallel\`, \`max_pct_missing\`, \`checkpoint_dir\`,
  \`shard\`).

- x:

  The object to print (the result of the function).

## Value

A list of class \`impute_tune_dataset\` with

- per_variable:

  named list of per-variable \[tune_impute_workflows()\] results
  (\`"workflows"\` engine), or the single sweep result (\`"missforest"\`
  / \`"missranger"\` engines, under \`sweep\`)

- summary:

  one row per imputed variable: \`variable\`, and for the
  \`"workflows"\` engine \`best_workflow_id\`, \`rmse\`, \`null_rmse\`,
  \`rse\` and \`n_imputed\`; for the forest engines \`best_mtry\` and
  \`best_error\`

- data:

  \`data\` with every target column filled in; for the forest engines,
  every incomplete column except \`exclude\`d ones (see the Forest
  engines section)

- engine, targets:

  the engine used and the variables imputed

## How the per-variable fits combine

Each variable is tuned against the \*\*original\*\* data (the other
columns as they stand, with the recipe's own median/mode imputation of
predictors), not against already-imputed values, so the result does not
depend on the order variables are tuned in. The completed data takes
each tuned variable's column from its own fit. Imputations are \*\*not
chained\*\*: an earlier target's tuned imputation is never used as a
predictor for a later target, and no sequential (chained-equations)
imputation is intended here. A variable that is complete, or that is not
numeric, is never a target for the \`"workflows"\` engine; the forest
engines impute every column they are given (numeric, factor and
character) except \`exclude\`d ones.

## Forest engines impute the whole data set

With \`engine = "missforest"\` or \`"missranger"\`, the sweep
(\[missforest_sweep_mtry()\] / \[missranger_sweep_mtry()\]) is run on
the whole data frame, not on \`targets\` alone, so \`\$data\` has
\*\*every\*\* incomplete column filled in — not only the columns named
in \`targets\` — except columns in \`exclude\` (and columns held out by
the sweep itself for exceeding \`max_pct_missing\`). \`\$summary\` still
reports one row per target only, so a non-target column that was filled
is not listed there. When \`targets\` is \`NULL\` (the default) this is
invisible, because every incomplete column is then a target; it only
matters when \`targets\` names a subset. If only the target columns
should be filled, use the \`"workflows"\` engine, or pass the non-target
columns in \`exclude\`.

## See also

\[tune_impute_workflows()\], \[missforest_sweep_mtry()\],
\[missranger_sweep_mtry()\]

## Examples

``` r
# \donttest{
ok <- vapply(c("workflowsets", "finetune", "tune", "rsample", "parsnip", "workflows", "yardstick"),
             requireNamespace, logical(1), quietly = TRUE)
if (all(ok)) {
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  d$hp[c(5, 15)] <- NA
  res <- tune_impute_dataset(d, folds = 3, race = FALSE)
  res$summary
}
#> # A tibble: 2 × 6
#>   variable best_workflow_id  rmse null_rmse   rse n_imputed
#>   <chr>    <chr>            <dbl>     <dbl> <dbl>     <int>
#> 1 mpg      base_lm           3.81      5.79 0.433         3
#> 2 hp       base_lm          36.8      73.6  0.250         2
# }
```
