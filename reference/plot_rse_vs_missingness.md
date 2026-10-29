# Plot Imputation Error Against the Percent Missing

Scatter of each method's relative squared error against how much of the
variable is missing, to see whether imputation gets worse as missingness
grows.

## Usage

``` r
plot_rse_vs_missingness(data, rse_df)
```

## Arguments

- data:

  The data frame with the missing values (used for the percent missing
  of each variable).

- rse_df:

  A \[compare_imputation_rse()\] result or anything with \`variable\`,
  \`method\`, \`rse\`. If it has a \`pct_miss\` column that is used
  instead of \`data\`.

## Value

A ggplot2 object. Variables of \`rse_df\` that are not columns of
\`data\` are dropped with a warning.
