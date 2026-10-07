# Tuning Parameters of a Ranked Workflow in a Workflow Set

Ranks the workflows of tuned workflow set results by \`rank_metric\`,
each workflow represented by its best configuration, and returns the
preprocessor, model, and best tuning parameters of the workflow at
position \`.rank\`.

## Usage

``` r
get_model_parameters(workflowset_results, rank_metric, .rank = 1)
```

## Arguments

- workflowset_results:

  Tuned workflow set results, the output of
  \[workflowsets::workflow_map()\].

- rank_metric:

  Name of the metric to rank by, e.g. \`"rmse"\` or
  \`"brier_survival_integrated"\`.

- .rank:

  Position to return; 1 (default) is the best workflow.

## Value

A one-row tibble with \`wflow_id\`, \`preproc\`, \`model\`, and one
column per tuning parameter.

## See also

\[fit_n_rank()\] to also fit the chosen configuration.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- workflowsets::workflow_map(wset, "tune_grid", resamples = folds, grid = 5)
get_model_parameters(results, "rmse")
get_model_parameters(results, "rmse", .rank = 2)
} # }
```
