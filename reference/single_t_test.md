# Single T-Test, Tidied

Runs a t-test comparing `.var` between the two levels of `.class`,
tidied into one row with group sizes/SDs and fold-change added.

## Usage

``` r
single_t_test(
  .data,
  .var,
  .class,
  .id = NULL,
  alternative = "two.sided",
  conf.level = 0.95,
  paired = FALSE,
  ...
)
```

## Arguments

- .data:

  A data frame or tibble.

- .var:

  Name (string) of the continuous column to test.

- .class:

  Name (string) of a binary (2-level) classification column.

- .id:

  Name (string, optional) of a subject/record identifier column. When
  `paired = TRUE`, pass this to pair observations by matching `.id`
  across the two groups rather than by row position — every id must have
  exactly one observation in each group. If `paired = TRUE` and `.id` is
  omitted, observations are paired by row order within each group, which
  silently produces meaningless results if the two groups aren't already
  sorted into corresponding order.

- alternative:

  One of `"two.sided"` (default), `"greater"`, or `"less"`.

- conf.level:

  Confidence level for the interval.

- paired:

  Logical; paired t-test.

- ...:

  Additional arguments passed to
  [`t.test`](https://rdrr.io/r/stats/t.test.html).

## Value

A one-row tibble.

## Examples

``` r
mtcars |>
  dplyr::mutate(am = factor(am)) |>
  single_t_test("mpg", "am")
#> # A tibble: 1 × 18
#>   estimate estimate1 estimate2 statistic p.value parameter conf.low conf.high
#>      <dbl>     <dbl>     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl>
#> 1    -7.24      17.1      24.4     -3.77 0.00137      18.3    -11.3     -3.21
#> # ℹ 10 more variables: method <chr>, alternative <chr>, var <chr>,
#> #   group1 <chr>, group2 <chr>, n_per_group <chr>, sd_per_group <chr>,
#> #   log_p <dbl>, fold_change <dbl>, log2_fold_change <dbl>
```
