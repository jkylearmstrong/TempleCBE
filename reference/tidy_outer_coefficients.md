# Tidy Outer Model Coefficients into Long Format

Unnests the \`coefficients\` list column produced by \[score_outer()\]
into a long tibble with one row per term per model and fold. Suitable
for plotting variable importance, coefficient paths, and term stability
across outer folds.

## Usage

``` r
tidy_outer_coefficients(x)

unnest_outer_coefficients(x)
```

## Arguments

- x:

  A tibble returned by \[score_outer()\].

## Value

A long tibble containing \`outer_fold\` (if present), \`model\`,
\`alpha\`, \`formula\` (if present), \`IBS\`, \`lambda\`, \`term\`, and
\`estimate\`.

## See also

\[score_outer()\]
