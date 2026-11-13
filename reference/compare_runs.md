# Compare and Diff Outer-Fold Runs Across Platforms With Tolerance

Evaluates reproducibility between two sets of model evaluation runs
(e.g. from \[score_outer()\]), such as runs performed across different
operating systems (Windows, Linux, macOS) or hardware architectures
(x86_64 vs ARM64). Compares integrated Brier scores (IBS), selected
penalties (\\\lambda\\), and coefficient estimates within user-specified
numeric tolerances.

## Usage

``` r
compare_runs(
  run1,
  run2,
  tolerance = 1e-06,
  coef_tolerance = tolerance,
  label1 = "run1",
  label2 = "run2"
)
```

## Arguments

- run1:

  A data frame returned by \[score_outer()\] or a character path to an
  \`.rds\` file.

- run2:

  A second data frame or character path to compare against \`run1\`.

- tolerance:

  Numeric tolerance for IBS and \\\lambda\\ differences (default
  \`1e-6\`).

- coef_tolerance:

  Numeric tolerance for coefficient differences (defaults to
  \`tolerance\`).

- label1:

  Optional character label for \`run1\` (default \`"run1"\`).

- label2:

  Optional character label for \`run2\` (default \`"run2"\`).

## Value

An object of class \`c("templecbe_run_comparison", "tbl_df", "tbl",
"data.frame")\` containing per-model diffs, with overall reproducibility
summary attributes.

## See also

\[score_outer()\], \[tidy_outer_coefficients()\]
