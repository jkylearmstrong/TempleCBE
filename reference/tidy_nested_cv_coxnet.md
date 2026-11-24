# Tidy the Outer-Fold Coefficients or LOCO-MP Importance of nested_cv_coxnet()

Unnests the \`.coefs\` or \`.importance\` list column of a
\[nested_cv_coxnet()\] result into a long tibble with one row per outer
split and term.

## Usage

``` r
tidy_nested_cv_coxnet(x, what = c("coefficients", "importance"))
```

## Arguments

- x:

  A \[nested_cv_coxnet()\] result.

- what:

  \`"coefficients"\` (default) unnests \`.coefs\`; \`"importance"\`
  unnests \`.importance\`, which is present when the run used
  \`importance = "loco_mp"\`.

## Value

For \`"coefficients"\`, a tibble with \`id\`, \`mixture\`, \`penalty\`,
\`term\` and \`estimate\`. For \`"importance"\`, a tibble with \`id\`,
\`mixture\`, \`penalty\`, \`term\`, \`importance\`, \`std_error\`,
\`statistic\`, \`p_value\`, \`p_adjusted\`, \`conf_low\` and
\`conf_high\`. An outer split whose LOCO-MP fit failed contributes no
rows.

## Details

\[tidy_outer_coefficients()\], \[summarize_outer_importance()\] and
\[plot_outer_importance()\] also accept a \`nested_cv_coxnet()\` result
directly: its \`id\`, \`mixture\`, \`penalty\` and \`.coefs\` are read
as the \`outer_fold\`, \`alpha\`, \`lambda\` and \`coefficients\` of
\[score_outer()\].

## See also

\[summarize_loco_mp()\], \[summarize_outer_importance()\],
\[nested_cv_coxnet()\]
