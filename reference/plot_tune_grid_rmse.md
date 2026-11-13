# Plot and Tabulate the Workflows of an Imputation Tuning Run

\`plot_tune_grid_rmse()\` shows the cross-validated RMSE (mean and one
standard error) of every tuning candidate, grouped by workflow, with the
null-model RMSE as a dashed line. \`tbl_tune_best_model()\` lists the
best candidate of each workflow with its RMSE and its RSE against the
null model.

## Usage

``` r
plot_tune_grid_rmse(x)

tbl_tune_best_model(x, digits = 3)
```

## Arguments

- x:

  A \[tune_impute_workflows()\] result.

- digits:

  Decimals in the table.

## Value

\`plot_tune_grid_rmse()\`: a \`ggplot2\` object.
\`tbl_tune_best_model()\`: a tibble with \`rank\`, \`workflow\`,
\`rmse\`, \`std_err\`, \`n_folds\`, \`null_rmse\`, \`rse\` and
\`selected\`.

## See also

\[tune_impute_workflows()\], \[plot_imputed_vs_observed()\]
