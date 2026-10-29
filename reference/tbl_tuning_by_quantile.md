# Tuning Results Summarised by Performance Quantile Tier

Bins tuning candidates into tiers by a performance metric (tier 1 is the
best) and summarises each tier: the number of candidates, the metric
range, the mean penalty (\`lambda\`) and mixing parameter (\`alpha\`),
or how often each model term occurs in the candidates of the tier.

## Usage

``` r
tbl_tuning_by_quantile(
  tuning_results,
  metric = "IBS",
  n_tiles = 5,
  lower_is_better = TRUE,
  what = c("summary", "terms")
)
```

## Arguments

- tuning_results:

  A data frame of candidates, or anything \[as_tuning_results()\]
  accepts. Needs the \`metric\` column; \`lambda\`, \`alpha\` and
  \`formula\` are used when present.

- metric:

  Name of the performance column (default \`"IBS"\`).

- n_tiles:

  Number of tiers (quantile groups of equal size, up to ties).

- lower_is_better:

  \`TRUE\` when a smaller metric is better (IBS, error); \`FALSE\`
  otherwise.

- what:

  \`"summary"\` (one row per tier) or \`"terms"\` (one row per tier and
  model term, with the number and share of the tier's candidates that
  contain it; needs a \`formula\` column).

## Value

A tibble. \`"summary"\`: \`tier\`, \`n_models\`, \`metric_min\`,
\`metric_mean\`, \`metric_max\`, \`mean_lambda\`, \`mean_alpha\` (the
last two \`NA\` when absent). \`"terms"\`: \`tier\`, \`term\`,
\`n_with_term\`, \`n_models\`, \`retention\`.

## See also

\[as_tuning_results()\], \[compare_tuning_runs()\],
\[select_candidate_models()\]

## Examples

``` r
set.seed(1)
tr <- data.frame(IBS = runif(50, 0.1, 0.3), alpha = runif(50), lambda = 10^runif(50, -3, 0),
                 formula = sample(c("~ a + b", "~ a + c", "~ b + c"), 50, TRUE))
tbl_tuning_by_quantile(tr, n_tiles = 4)
#> # A tibble: 4 × 7
#>    tier n_models metric_min metric_mean metric_max mean_lambda mean_alpha
#>   <int>    <int>      <dbl>       <dbl>      <dbl>       <dbl>      <dbl>
#> 1     1       12      0.103       0.131      0.153      0.135       0.465
#> 2     2       13      0.168       0.188      0.211      0.0434      0.525
#> 3     3       12      0.215       0.233      0.246      0.186       0.462
#> 4     4       13      0.254       0.271      0.298      0.215       0.555
tbl_tuning_by_quantile(tr, n_tiles = 4, what = "terms")
#> # A tibble: 12 × 5
#>     tier term  n_with_term n_models retention
#>    <int> <chr>       <int>    <int>     <dbl>
#>  1     1 a              10       12     0.833
#>  2     1 b               8       12     0.667
#>  3     1 c               6       12     0.5  
#>  4     2 c              13       13     1    
#>  5     2 b              10       13     0.769
#>  6     2 a               3       13     0.231
#>  7     3 a               8       12     0.667
#>  8     3 b               8       12     0.667
#>  9     3 c               8       12     0.667
#> 10     4 a              10       13     0.769
#> 11     4 b               8       13     0.615
#> 12     4 c               8       13     0.615
```
