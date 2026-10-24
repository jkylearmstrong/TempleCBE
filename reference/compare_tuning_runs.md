# Compare and Summarize Two Tuning Runs

Computes comparative summary statistics (mean, median, 25th and 75th
percentiles) of integrated Brier scores (IBS) across candidate alpha
values and formulas between two tuning runs (e.g. from
\[tune_alpha_by_split()\] or \[tune_over_alpha()\]).

## Usage

``` r
compare_tuning_runs(
  tune1,
  tune2,
  tolerance = 1e-04,
  label1 = "run1",
  label2 = "run2",
  match = c("exact", "nearest", "binned"),
  n_bins = 10
)
```

## Arguments

- tune1:

  A data frame of tuning results containing \`alpha\` and \`IBS\`
  columns, or a path to an \`.rds\` file.

- tune2:

  A second data frame of tuning results to compare against \`tune1\`, or
  a path to an \`.rds\` file.

- tolerance:

  Numeric tolerance for differences in mean IBS (default \`1e-4\`).

- label1:

  Character label for \`tune1\` (default \`"run1"\`).

- label2:

  Character label for \`tune2\` (default \`"run2"\`).

- match:

  Matching strategy for candidate hyperparameter configurations:
  \`"exact"\` (default; matches on exact \`alpha\` values),
  \`"nearest"\` (matches each \`alpha\` in \`tune1\` to the closest
  \`alpha\` in \`tune2\`), or \`"binned"\` (discretizes \`alpha\` into
  \`n_bins\` equal-width bins).

- n_bins:

  Number of bins when \`match = "binned"\` (default \`10\`).

## Value

An object of class \`c("templecbe_tuning_comparison", "tbl_df", "tbl",
"data.frame")\` summarizing per-alpha IBS distributions, distributional
shifts, and global IBS statistics.

## See also

\[tune_alpha_by_split()\], \[compare_runs()\]
