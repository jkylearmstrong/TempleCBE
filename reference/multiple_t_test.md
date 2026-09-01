# Multiple T-Tests Against One Classifier

Runs
[`single_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md)
for every (or a chosen set of) numeric column against a single binary
classifier.

## Usage

``` r
multiple_t_test(
  .data,
  .var_list = names(dplyr::select(.data, dplyr::where(is.numeric))),
  .class,
  alternative = "two.sided",
  conf.level = 0.95,
  ...
)
```

## Arguments

- .data:

  A data frame or tibble.

- .var_list:

  Character vector of column names to test (default: every numeric
  column in `.data`).

- .class:

  Name (string) of a binary classification column.

- alternative:

  One of `"two.sided"` (default), `"greater"`, or `"less"`.

- conf.level:

  Confidence level for the interval.

- ...:

  Additional arguments passed to
  [`single_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md)
  (and on to [`t.test`](https://rdrr.io/r/stats/t.test.html)) — e.g.
  `paired` and `.id`.

## Value

A tibble with one row per tested variable.

## Examples

``` r
mtcars |>
  dplyr::mutate(am = factor(am)) |>
  multiple_t_test(.class = "am")
#> # A tibble: 10 × 18
#>    estimate estimate1 estimate2 statistic   p.value parameter conf.low conf.high
#>       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#>  1   -7.24     17.1      24.4      -3.77    1.37e-3      18.3  -11.3      -3.21 
#>  2    1.87      6.95      5.08      3.35    2.46e-3      25.9    0.724     3.02 
#>  3  147.      290.      144.        4.20    2.30e-4      29.3   75.3     218.   
#>  4   33.4     160.      127.        1.27    2.21e-1      18.7  -21.9      88.7  
#>  5   -0.764     3.29      4.05     -5.65    5.27e-6      27.2   -1.04     -0.486
#>  6    1.36      3.77      2.41      5.49    6.27e-6      29.2    0.853     1.86 
#>  7    0.823    18.2      17.4       1.29    2.09e-1      25.5   -0.492     2.14 
#>  8   -0.170     0.368     0.538    -0.927   3.63e-1      25.1   -0.548     0.208
#>  9   -1.17      3.21      4.38     -6.90    5.46e-7      22.6   -1.53     -0.822
#> 10   -0.186     2.74      2.92     -0.283   7.81e-1      16.6   -1.58      1.21 
#> # ℹ 10 more variables: method <chr>, alternative <chr>, var <chr>,
#> #   group1 <chr>, group2 <chr>, n_per_group <chr>, sd_per_group <chr>,
#> #   log_p <dbl>, fold_change <dbl>, log2_fold_change <dbl>
```
