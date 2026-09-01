# Find Highly Correlated Columns

Identifies numeric columns to drop for multicollinearity, via
[`findCorrelation`](https://rdrr.io/pkg/caret/man/findCorrelation.html)
on the pairwise correlation matrix of `data`'s numeric columns. Named
`find_correlation()` (not `findCorrelation()`) so it doesn't shadow
caret's function of the same name for anyone with both packages loaded.

## Usage

``` r
find_correlation(
  data,
  use = "pairwise.complete.obs",
  method = "pearson",
  cutoff = 0.9,
  verbose = FALSE,
  names = TRUE,
  exact = ncol(data) < 100
)
```

## Arguments

- data:

  A data frame or tibble.

- use:

  Passed to [`cor`](https://rdrr.io/r/stats/cor.html) (default
  `"pairwise.complete.obs"`).

- method:

  Passed to [`cor`](https://rdrr.io/r/stats/cor.html) (default
  `"pearson"`).

- cutoff:

  Absolute correlation above which a column is flagged (default 0.9).

- verbose:

  Logical; passed to
  [`findCorrelation`](https://rdrr.io/pkg/caret/man/findCorrelation.html).

- names:

  Logical; if `TRUE` (default) return column names instead of indices.

- exact:

  Passed to
  [`findCorrelation`](https://rdrr.io/pkg/caret/man/findCorrelation.html);
  defaults to `ncol(data) < 100` (the original had a bug here
  referencing an undefined `x` instead of `data` — fixed).

## Value

Character vector (or integer indices) of columns to remove.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  find_correlation(mtcars, cutoff = 0.8)
}
#> [1] "cyl"  "disp" "mpg" 
```
