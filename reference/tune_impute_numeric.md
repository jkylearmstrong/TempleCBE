# Benchmark Numeric Imputation Methods Using Relative Squared Error (RSE)

Evaluates multiple numeric imputation methods (mean, median, and KNN) by
artificially masking observed values and computing Relative Squared
Error (RSE) and MAE.

## Usage

``` r
tune_impute_numeric(
  data,
  target_cols,
  methods = c("mean", "median", "knn"),
  prop_mask = 0.15,
  seed = 123,
  checkpoint_dir = NULL,
  shard = NULL,
  checkpoint_mismatch = c("recompute", "overwrite", "error")
)
```

## Arguments

- data:

  A data frame with numeric columns.

- target_cols:

  Character vector of numeric columns to evaluate.

- methods:

  Imputation methods to benchmark: \`"mean"\`, \`"median"\`, and/or
  \`"knn"\`.

- prop_mask:

  Proportion of observed values to artificially mask (default 0.15).

- seed:

  Random seed for reproducible masking.

- checkpoint_dir:

  Optional directory path where per-task checkpoints are stored.

- shard:

  Optional shard specification (e.g. \`c(1, 2)\`) for distributed
  computation.

- checkpoint_mismatch:

  Action on mismatch: \`"recompute"\`, \`"overwrite"\`, or \`"error"\`.

## Value

A tibble ranking the imputation methods by RSE and MAE across variables.
