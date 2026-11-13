# All Formulas with k Terms

Exhaustively expands the combinations of \`features\`. The number of
formulas is \`choose(n, k)\` summed over the sizes, which grows quickly
(20 features, sizes 2 to 20, is over a million), so the call stops above
\`max_formulas\`.

## Usage

``` r
expand_feature_formulas(
  features,
  k = NULL,
  min_terms = 2,
  max_terms = length(features),
  response = NULL,
  max_formulas = 1e+05
)
```

## Arguments

- features:

  Character vector of terms.

- k:

  One size, or \`NULL\` for the range \`min_terms\` to \`max_terms\`.

- min_terms, max_terms:

  Size range when \`k\` is \`NULL\`.

- response:

  Optional left-hand side as text, for example \`"Surv(time, status)"\`
  or \`"outcome"\`; a trailing \`~\` is accepted.

- max_formulas:

  Safety limit on the number of formulas.

## Value

A tibble with \`id\`, \`formula\` (list of formulas), \`terms\` (list of
character vectors) and \`n_terms\`, ordered by size and then by the
order of \`features\`.

## See also

\[sample_weighted_formulas()\], \[sample_candidate_formulas()\]

## Examples

``` r
expand_feature_formulas(c("a", "b", "c"), k = 2, response = "y ~")
#> # A tibble: 3 × 4
#>      id formula   terms     n_terms
#>   <int> <list>    <list>      <int>
#> 1     1 <formula> <chr [2]>       2
#> 2     2 <formula> <chr [2]>       2
#> 3     3 <formula> <chr [2]>       2
expand_feature_formulas(c("a", "b", "c"))
#> # A tibble: 4 × 4
#>      id formula   terms     n_terms
#>   <int> <list>    <list>      <int>
#> 1     1 <formula> <chr [2]>       2
#> 2     2 <formula> <chr [2]>       2
#> 3     3 <formula> <chr [2]>       2
#> 4     4 <formula> <chr [3]>       3
```
