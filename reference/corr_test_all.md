# Pairwise Correlation Tests Across All Numeric Columns

Runs [`cor.test`](https://rdrr.io/r/stats/cor.test.html) on every pair
of numeric columns in a data frame and returns one row per pair.

## Usage

``` r
corr_test_all(
  data,
  method = "pearson",
  use = c("pairwise.complete.obs", "complete.obs"),
  columns = c("compact", "tidy"),
  sort = c("p_value", "estimate", "abs_estimate", "none"),
  ...
)
```

## Arguments

- data:

  A data frame or tibble. Non-numeric columns are ignored.

- method:

  Correlation method passed to
  [`cor.test`](https://rdrr.io/r/stats/cor.test.html): `"pearson"`
  (default), `"kendall"`, or `"spearman"`.

- use:

  How missing values are handled. `"pairwise.complete.obs"` (default)
  tests each pair on the rows where both columns are observed, which is
  what [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) does
  natively. `"complete.obs"` first drops every row with a missing value
  in any numeric column, so all pairs are tested on the same rows.

- columns:

  Output shape. `"compact"` (default) returns `var1`, `var2`, `r`,
  `p_value`. `"tidy"` returns `var1`, `var2`, then every column
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  reports for the test, with `estimate` renamed `cor`: `cor`,
  `statistic`, `p.value`, `parameter`, `conf.low`, `conf.high`,
  `method`, `alternative`. `parameter` and the confidence limits are
  only reported by methods that compute them (Pearson).

- sort:

  Row order. `"p_value"` (default) is ascending p-value; `"estimate"` is
  descending correlation; `"abs_estimate"` is descending absolute
  correlation; `"none"` keeps pair order. Ties keep pair order, and
  `NA`s sort last.

- ...:

  Further arguments passed to
  [`cor.test`](https://rdrr.io/r/stats/cor.test.html), such as
  `alternative`, `conf.level`, or `exact`.

## Value

A tibble with one row per pair of numeric columns.

## Details

Pairs are enumerated in column order (via
[`combn`](https://rdrr.io/r/utils/combn.html)), so `var1` is always the
column that appears first in `data`. A pair whose test cannot be
computed (e.g. fewer than three complete observations) is kept with `NA`
results rather than aborting the whole run.

## Examples

``` r
corr_test_all(iris[, 1:4])
#> # A tibble: 6 × 4
#>   var1         var2              r  p_value
#>   <chr>        <chr>         <dbl>    <dbl>
#> 1 Petal.Length Petal.Width   0.963 4.68e-86
#> 2 Sepal.Length Petal.Length  0.872 1.04e-47
#> 3 Sepal.Length Petal.Width   0.818 2.33e-37
#> 4 Sepal.Width  Petal.Length -0.428 4.51e- 8
#> 5 Sepal.Width  Petal.Width  -0.366 4.07e- 6
#> 6 Sepal.Length Sepal.Width  -0.118 1.52e- 1

# Every cor.test() statistic, strongest positive correlation first
corr_test_all(mtcars[, c("mpg", "hp", "wt", "qsec")], columns = "tidy", sort = "estimate")
#> # A tibble: 6 × 10
#>   var1  var2     cor statistic  p.value parameter conf.low conf.high method     
#>   <chr> <chr>  <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>      
#> 1 hp    wt     0.659     4.80  4.15e- 5        30   0.403      0.819 Pearson's …
#> 2 mpg   qsec   0.419     2.53  1.71e- 2        30   0.0820     0.670 Pearson's …
#> 3 wt    qsec  -0.175    -0.972 3.39e- 1        30  -0.493      0.185 Pearson's …
#> 4 hp    qsec  -0.708    -5.49  5.77e- 6        30  -0.848     -0.477 Pearson's …
#> 5 mpg   hp    -0.776    -6.74  1.79e- 7        30  -0.885     -0.586 Pearson's …
#> 6 mpg   wt    -0.868    -9.56  1.29e-10        30  -0.934     -0.744 Pearson's …
#> # ℹ 1 more variable: alternative <chr>
```
