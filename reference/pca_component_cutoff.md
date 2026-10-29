# Number of Components Needed to Reach a Cumulative Variance Threshold

Number of Components Needed to Reach a Cumulative Variance Threshold

## Usage

``` r
pca_component_cutoff(pca_mod, threshold = 0.8)
```

## Arguments

- pca_mod:

  A \[stats::prcomp()\] object or a \[run_pca()\] result.

- threshold:

  Cumulative share of the total variance, in (0, 1\] (default 0.80).

## Value

The smallest number of components whose cumulative variance share is at
least \`threshold\`, as an integer with the attribute \`cumulative\`
(the shares for all components). For \`prcomp(..., rank. = k)\` the
shares are of the full variance; a threshold that the retained
components cannot reach is an error.

## See also

\[pca_variance_pct()\], \[pca_equation_latex()\]

## Examples

``` r
pca_component_cutoff(prcomp(mtcars, scale. = TRUE), 0.9)
#> [1] 4
#> attr(,"cumulative")
#>  [1] 0.6007637 0.8417153 0.8987332 0.9232421 0.9435558 0.9627918 0.9750884
#>  [8] 0.9862612 0.9932655 0.9979960 1.0000000
```
