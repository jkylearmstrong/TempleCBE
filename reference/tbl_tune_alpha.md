# Summary Table of Tuning Performance Across Alpha Values

Calculates summary statistics (mean, SD, median, IQR, min, max) of the
Integrated Brier Score for each evaluated \`alpha\` value.

## Usage

``` r
tbl_tune_alpha(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[summarize_tune_results()\].

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A tibble of per-alpha IBS statistics.
