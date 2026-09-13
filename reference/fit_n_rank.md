# Fit the Configuration Ranked \`.rank\` in a Workflow Set

Ranks the configurations of tuned workflow set results by
\`rank_metric\`, picks the one at position \`.rank\`, and fits it on the
full training set with \[tune::fit_best()\].

## Usage

``` r
fit_n_rank(
  grid_results,
  rank_metric,
  .rank = 1,
  group_wflow = TRUE,
  select_best = FALSE,
  ...
)
```

## Arguments

- grid_results:

  Tuned workflow set results, the output of
  \[workflowsets::workflow_map()\] run with \`control =
  tune::control_grid(save_workflow = TRUE)\`, which \[tune::fit_best()\]
  needs.

- rank_metric:

  Name of the metric to rank by.

- .rank:

  Position to fit; 1 (default) is the best.

- group_wflow:

  If \`TRUE\` (default), rank workflows, each by its best configuration,
  so \`.rank = 2\` is the second-best \*workflow\*. If \`FALSE\`, rank
  configurations directly, so \`.rank = 2\` can be a second
  configuration of the best workflow.

- select_best:

  Passed to \[workflowsets::rank_results()\]: keep only each workflow's
  best configuration before ranking.

- ...:

  Passed to \[tune::fit_best()\].

## Value

A list with \`model\`, the fitted workflow, and \`tuned_parameters\`, a
one-row tibble of the workflow's \`wflow_id\`, \`preproc\`, \`model\`,
tuning parameters, and its \[workflowsets::rank_results()\] row
(\`.config\`, \`.metric\`, \`mean\`, \`std_err\`, \`n\`, \`rank\`, and
\`overall_rank\` when \`group_wflow = TRUE\`).

## See also

\[get_model_parameters()\]

## Examples

``` r
if (FALSE) { # \dontrun{
results <- workflowsets::workflow_map(
  wset, "tune_grid", resamples = folds, grid = 5,
  control = tune::control_grid(save_workflow = TRUE)
)
best <- fit_n_rank(results, rank_metric = "rmse")
best$tuned_parameters
predict(best$model, new_data)
} # }
```
