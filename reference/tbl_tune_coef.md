# Summary Table of Coefficient Stability Across Tuning Resamples

Calculates selection frequency and distribution of coefficient estimates
for each feature across resamples.

## Usage

``` r
tbl_tune_coef(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[summarize_tune_results()\].

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A tibble of feature coefficient stability statistics.
