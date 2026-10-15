# Select and Finalize Best Penalized Cox Workflow

Runs cross-validation tuning across a grid of elastic net mixture and
penalty parameters and finalizes the workflow with the best-performing
hyperparameters.

## Usage

``` r
tune_best_glmnet_workflow(
  formula_or_recipe,
  data,
  resamples = NULL,
  mixture = c(0.2, 0.5, 0.8, 1),
  rule = c("min", "1se"),
  ...
)
```

## Arguments

- formula_or_recipe:

  A formula with \`Surv()\` outcome or a \`recipes::recipe\`.

- data:

  Data frame containing predictors and survival outcomes.

- resamples:

  An \`rsample\` resample object (e.g. \`vfold_cv\` or
  \`group_vfold_cv\`).

- mixture:

  Numeric vector of candidate alpha mixture values (default \`c(0.2,
  0.5, 0.8, 1.0)\`).

- rule:

  Hyperparameter selection rule: \`"min"\` (default) or \`"1se"\`.

- ...:

  Additional arguments passed to \[cv_coxnet()\].

## Value

A list containing \`best_mixture\`, \`best_penalty\`, \`model\` (fitted
on full data), and \`cv_results\`.
