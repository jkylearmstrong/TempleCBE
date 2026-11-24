# Summary Table of Coefficient Stability Across Tuning Resamples

Calculates selection frequency and distribution of coefficient estimates
for each feature across resamples.

## Usage

``` r
tbl_tune_coef(data, label_map = NULL, style = NULL)
```

## Arguments

- data:

  A data frame from \[tune_alpha_by_split()\].

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A tibble of feature coefficient stability statistics.

## Details

\`prop_selected\` is \`n_selected\` divided by the number of fits of the
term (distinct resampling-split by \`alpha\` combinations when \`data\`
identifies them with a column \`inner_resamples_splits\`,
\`inner_resamples\` or \`split\`; without such a column it is the share
of the fits of each term with a non-zero estimate, and a message says
so). Dividing by the number of splits alone would overstate the
proportion whenever several alphas are tuned per split.
