# Correlations Within and Between Feature Domains

Lists pairs of features whose absolute correlation is at least
\`cutoff\`, labelled by the domain of each feature (for example blood
gas, pulmonary, cardiovascular) and by whether the pair lies within one
domain or between two. See \[tbl_correlated_pairs()\] for a table
without domains.

## Usage

``` r
tbl_domain_correlations(
  data,
  domain_map,
  cutoff = 0.5,
  cross_domain_only = FALSE,
  method = c("pearson", "spearman")
)
```

## Arguments

- data:

  A data frame.

- domain_map:

  A data frame with columns \`variable\` (or \`columns\`) and
  \`domain\`, or a named vector (names = columns, values = domains).
  Only mapped numeric columns of \`data\` are used; a mapped column that
  is not in \`data\` is an error.

- cutoff:

  Absolute correlation threshold in (0, 1\]; a pair at exactly the
  cutoff is listed.

- cross_domain_only:

  Keep only pairs from different domains.

- method:

  \`"pearson"\` or \`"spearman"\` (pairwise complete observations).

## Value

A tibble with \`feature_1\`, \`domain_1\`, \`feature_2\`, \`domain_2\`,
\`scope\` (\`"within"\` or \`"between"\`), \`correlation\` and \`n\`
(pairs of observations used), sorted by decreasing absolute correlation.
Pairs whose correlation cannot be computed are not listed. The attribute
\`domain_summary\` counts the listed pairs per domain pair.

## See also

\[tbl_correlated_pairs()\], \[corr_test_all()\]

## Examples

``` r
map <- c(mpg = "engine", disp = "engine", hp = "engine", wt = "body", qsec = "body", drat = "body")
tbl_domain_correlations(mtcars, map, cutoff = 0.7)
#> # A tibble: 8 × 7
#>   feature_1 domain_1 feature_2 domain_2 scope   correlation     n
#>   <chr>     <chr>    <chr>     <chr>    <chr>         <dbl> <int>
#> 1 disp      engine   wt        body     between       0.888    32
#> 2 mpg       engine   wt        body     between      -0.868    32
#> 3 mpg       engine   disp      engine   within       -0.848    32
#> 4 disp      engine   hp        engine   within        0.791    32
#> 5 mpg       engine   hp        engine   within       -0.776    32
#> 6 wt        body     drat      body     within       -0.712    32
#> 7 disp      engine   drat      body     between      -0.710    32
#> 8 hp        engine   qsec      body     between      -0.708    32
tbl_domain_correlations(mtcars, map, cutoff = 0.7, cross_domain_only = TRUE)
#> # A tibble: 4 × 7
#>   feature_1 domain_1 feature_2 domain_2 scope   correlation     n
#>   <chr>     <chr>    <chr>     <chr>    <chr>         <dbl> <int>
#> 1 disp      engine   wt        body     between       0.888    32
#> 2 mpg       engine   wt        body     between      -0.868    32
#> 3 disp      engine   drat      body     between      -0.710    32
#> 4 hp        engine   qsec      body     between      -0.708    32
```
