# Exact Test for 2x2 Tables with Automatic Zero-Cell Mid-p Default

Performs an exact test for 2x2 contingency tables using exact2x2. If any
cell count in the 2x2 table is zero (`min(tab) == 0`) and `midp` is
unspecified, the function defaults to the mid-p version of Central
Fisher's exact test (`midp = TRUE`). This prevents the extreme
conservatism and loss of power of standard conditional exact tests on
boundary tables. When all cells are non-zero, it defaults to the
standard Central Fisher's exact test (`midp = FALSE`), which guarantees
central confidence intervals that invert the two one-sided tests.

## Usage

``` r
cbe_exact2x2(
  x,
  y = NULL,
  data = NULL,
  variable = NULL,
  by = NULL,
  midp = NULL,
  conf.level = 0.95,
  alternative = "two.sided",
  tsmethod = "central",
  ...
)
```

## Arguments

- x:

  A 2x2 numeric matrix, table, or a categorical vector. Can also be a
  data frame if `variable` and `by` are supplied.

- y:

  Optional second categorical vector when `x` is a vector.

- data:

  Optional data frame when used with `variable` and `by`.

- variable:

  Character string of the column name to test when called by gtsummary.

- by:

  Character string of the grouping/stratifying column when called by
  gtsummary.

- midp:

  Logical. If `NULL` (the default), automatically set to `TRUE` when any
  table cell is 0, and `FALSE` when all cells are positive. Can be
  explicitly set to `TRUE` or `FALSE` to override.

- conf.level:

  Confidence level for the returned confidence interval (default 0.95).

- alternative:

  Alternative hypothesis direction: `"two.sided"` (default),
  `"greater"`, or `"less"`.

- tsmethod:

  Two-sided method passed to
  [`exact2x2`](https://rdrr.io/pkg/exact2x2/man/exact2x2.html):
  `"two.sided"` (Central Fisher's exact test, default), `"minlike"`, or
  `"blaker"`.

- ...:

  Additional arguments passed to
  [`exact2x2`](https://rdrr.io/pkg/exact2x2/man/exact2x2.html).

## Value

A tibble with columns:

- estimate:

  Estimated odds ratio (conditional MLE or median unbiased estimate).

- p.value:

  Two-sided or one-sided p-value (mid-p adjusted when `midp = TRUE`).

- conf.low:

  Lower bound of the confidence interval.

- conf.high:

  Upper bound of the confidence interval.

- statistic:

  Point estimate of the odds ratio (for gtsummary compatibility).

- method:

  Descriptive name of the exact test performed.

- alternative:

  Alternative hypothesis.

- midp:

  Logical flag indicating whether mid-p adjustment was used.

- has_zero:

  Logical flag indicating whether any cell in the table was zero.

## Details

The function accepts a 2x2 table/matrix, two categorical vectors `x` and
`y`, or `(data, variable, by)` arguments for direct compatibility as a
custom test in gtsummary's
[`add_p`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html).

## Examples

``` r
# 2x2 table with a zero cell: automatically triggers mid-p adjustment
tab_zero <- matrix(c(0, 10, 5, 15), nrow = 2,
                   dimnames = list(c("Treated", "Control"), c("Event", "No Event")))
cbe_exact2x2(tab_zero)
#> # A tibble: 1 × 9
#>   estimate p.value conf.low conf.high statistic method         alternative midp 
#>      <dbl>   <dbl>    <dbl>     <dbl>     <dbl> <chr>          <chr>       <lgl>
#> 1        0   0.109        0      1.52         0 Central Fishe… two.sided   TRUE 
#> # ℹ 1 more variable: has_zero <lgl>

# 2x2 table without zero cells: standard Central Fisher's exact test
tab_nonzero <- matrix(c(12, 8, 5, 15), nrow = 2)
cbe_exact2x2(tab_nonzero)
#> # A tibble: 1 × 9
#>   estimate p.value conf.low conf.high statistic method         alternative midp 
#>      <dbl>   <dbl>    <dbl>     <dbl>     <dbl> <chr>          <chr>       <lgl>
#> 1     4.32  0.0536    0.982      21.9      4.32 Central Fishe… two.sided   FALSE
#> # ℹ 1 more variable: has_zero <lgl>
```
