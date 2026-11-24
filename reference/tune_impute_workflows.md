# Tune Candidate Workflows to Impute One Numeric Variable

Benchmarks every combination of candidate recipes and models for
predicting one numeric variable from the others, by cross-validated
RMSE, and uses the best one to fill in the variable's missing values. It
wraps the \`workflowsets::workflow_set()\` plus
\`finetune::tune_race_anova()\` scaffold that was repeated for each
imputed variable.

## Usage

``` r
tune_impute_workflows(
  data,
  target,
  recipes = NULL,
  models = NULL,
  folds = 5,
  grid = 10,
  race = TRUE,
  seed = 123,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame.

- target:

  Name of the numeric column to impute.

- recipes:

  Named list of \`recipes::recipe()\` objects with \`target\` as the
  outcome, or \`NULL\` for one default recipe (all other columns as
  predictors; median / mode imputation of the predictors, dummy
  variables, zero-variance filter).

- models:

  Named list of \`parsnip\` regression model specifications, or \`NULL\`
  for a linear model (\`parsnip::linear_reg()\`).

- folds:

  Number of cross-validation folds (default 5), or an \`rsample\`
  resampling object built on the rows of \`data\` where \`target\` is
  observed.

- grid:

  Number of tuning candidates per workflow (default 10), or a grid data
  frame.

- race:

  If \`TRUE\` (default), tune with \`finetune::tune_race_anova()\`
  (needs at least three folds); otherwise with \`tune::tune_grid()\`.
  Workflows without tuning parameters are simply resampled.

- seed:

  Random seed (default 123).

- verbose:

  Passed to \`workflowsets::workflow_map()\`.

## Value

A list of class \`cbe_impute_tune\` with

- ranking:

  one row per workflow (its best candidate): \`wflow_id\`, \`rmse\`,
  \`std_err\`, \`n\`, \`rank\`, ordered best first

- metrics:

  RMSE of every candidate of every workflow

- best_workflow_id, best_params:

  the winner and its tuning parameter values

- predictions:

  out-of-fold predictions of the winner: \`Fold\`, \`.row\` (row of
  \`data\`), \`observed\` and \`imputed\`, ready for
  \[plot_imputed_vs_observed()\]

- rmse, null_rmse, rse:

  see Null model and RSE

- fit:

  the winner fitted to all rows with an observed \`target\`

- data:

  \`data\` with the missing values of \`target\` filled in

- n_imputed, target:

  number of filled values, and the target name

## What is tuned on

Only rows where \`target\` is observed are used for resampling and for
the final fit; rows where it is missing are predicted at the end.
Predictors may have missing values only if the recipe deals with them
(the default recipe imputes them with the median or mode).

## Null model and RSE

\`null_rmse\` is the cross-validated RMSE of predicting every held-out
value with the mean of \`target\` in the corresponding training folds,
and \`rse = (rmse / null_rmse)^2\` for the best workflow, both from the
same folds. An RSE below 1 beats the mean. This is the training-mean
baseline of \[tune_impute_numeric()\], not the oracle baseline of
\[compare_imputation_rse()\].

## Cost and reproducibility

Every workflow is tuned over \`grid\` candidates on every fold, so the
run time grows with \`length(recipes) \* length(models) \* grid \*
folds\`. Racing drops poor candidates after the first three folds.
Results depend on \`seed\` (folds, grids and models that use random
numbers).

## See also

\[plot_tune_grid_rmse()\], \[tbl_tune_best_model()\],
\[plot_imputed_vs_observed()\], \[tune_impute_numeric()\],
\[compare_imputation_rse()\]

## Examples

``` r
# \donttest{
ok <- vapply(c("workflowsets", "finetune", "tune", "rsample", "parsnip", "workflows", "yardstick"),
             requireNamespace, logical(1), quietly = TRUE)
if (all(ok)) {
  d <- mtcars
  d$mpg[c(3, 11, 20)] <- NA
  res <- tune_impute_workflows(d, "mpg", folds = 3, race = FALSE)
  tbl_tune_best_model(res)
}
#> # A tibble: 1 × 8
#>    rank workflow  rmse std_err n_folds null_rmse   rse selected
#>   <int> <chr>    <dbl>   <dbl>   <int>     <dbl> <dbl> <lgl>   
#> 1     1 base_lm   3.81   0.219       3      5.79 0.433 TRUE    
# }
```
