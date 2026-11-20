# Plot Imputation Performance Across Columns

Dot plot of each method's relative squared error (or RMSE) by variable,
with the null baseline (RSE = 1) marked.

## Usage

``` r
plot_rse_by_column(rse_df, metric = c("RSE", "RMSE"), log_scale = FALSE)
```

## Arguments

- rse_df:

  A \[compare_imputation_rse()\] result or anything with \`variable\`,
  \`method\`, \`rse\`.

- metric:

  \`"RSE"\` (default) or \`"RMSE"\` (needs an \`rmse\` column).

- log_scale:

  Logical; plot RSE on a log axis (default \`FALSE\`).

## Value

A ggplot2 object.

## Examples

``` r
rse <- data.frame(variable = c("x", "x", "y", "y"),
                  method = c("mean", "knn", "mean", "knn"), rse = c(1, 0.4, 0.9, 1.2))
plot_rse_by_column(rse)
```
