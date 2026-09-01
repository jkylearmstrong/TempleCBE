# One-vs-Rest T-Tests Across a Multi-Level Factor

For a classifier with more than two levels, dichotomizes each level
against all others in turn and runs
[`single_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md).

## Usage

``` r
one_vs_rest_t_test(
  .data,
  .var,
  .class,
  .id = NULL,
  paired = FALSE,
  alternative = "two.sided",
  conf.level = 0.95
)
```

## Arguments

- .data:

  A data frame or tibble.

- .var:

  Name (string) of the continuous column to test.

- .class:

  Name (string) of a classification column (2+ levels).

- .id:

  Name (string, optional) of a subject/record identifier column; see
  [`single_t_test`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md).
  Only meaningful when `paired = TRUE`.

- paired:

  Logical; paired t-test.

- alternative:

  One of `"two.sided"` (default), `"greater"`, or `"less"`.

- conf.level:

  Confidence level for the interval.

## Value

A tibble with one row per level of `.class`.

## Examples

``` r
one_vs_rest_t_test(iris, "Sepal.Length", "Species")
#> # A tibble: 3 × 18
#>   estimate estimate1 estimate2 statistic  p.value parameter conf.low conf.high
#>      <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl>
#> 1   -1.26       5.01      6.26    -15.1  7.71e-32     147.   -1.42      -1.09 
#> 2    0.139      5.94      5.80      1.16 2.46e- 1     147.   -0.0970     0.375
#> 3    1.12       6.59      5.47     10.1  6.32e-17      98.9   0.898      1.34 
#> # ℹ 10 more variables: method <chr>, alternative <chr>, var <chr>,
#> #   group1 <chr>, group2 <chr>, n_per_group <chr>, sd_per_group <chr>,
#> #   log_p <dbl>, fold_change <dbl>, log2_fold_change <dbl>
```
