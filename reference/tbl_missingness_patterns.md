# Missingness Patterns Table

Counts the distinct combinations of missing and observed values across
columns, one row per pattern, most common first. It is the tabular
companion to \[plot_comissingness_matrix()\] and a pre-flight check
before multiple imputation (see \[pool_imputations()\]): it shows which
columns tend to be missing together and how many rows are complete.

## Usage

``` r
tbl_missingness_patterns(data, vars = NULL)
```

## Arguments

- data:

  A data frame.

- vars:

  Columns to use (default all).

## Value

A tibble with one logical column per variable (\`TRUE\` = missing), then
\`n_missing_vars\`, \`n\` (rows with the pattern) and \`percent\` (of
all rows).

## See also

\[plot_comissingness_matrix()\], \[features_percent_miss()\]

## Examples

``` r
d <- data.frame(a = c(1, NA, 3, NA, 5, 6), b = c(1, NA, 3, NA, 5, NA), c = c(NA, 2, 3, 4, 5, 6))
tbl_missingness_patterns(d)
#> # A tibble: 4 × 6
#>   a     b     c     n_missing_vars     n percent
#>   <lgl> <lgl> <lgl>          <int> <int>   <dbl>
#> 1 FALSE FALSE FALSE              0     2    33.3
#> 2 TRUE  TRUE  FALSE              2     2    33.3
#> 3 FALSE FALSE TRUE               1     1    16.7
#> 4 FALSE TRUE  FALSE              1     1    16.7
```
