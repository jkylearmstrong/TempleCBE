# Compact Groupwise Numeric Summary

Raw numeric summaries (n, missingness, mean, SD, median, quartiles, IQR,
range) of numeric variables, overall or by one or more grouping columns,
as a long tibble for programmatic filtering. No formatting or rounding:
use \[summary_table()\] or \[tbl_table_one()\] for presentation.

## Usage

``` r
calc_grouped_summary(
  data,
  vars = NULL,
  by = NULL,
  stats = c("n", "n_missing", "pct_missing", "mean", "sd", "median", "q1", "q3", "iqr",
    "min", "max")
)
```

## Arguments

- data:

  A data frame.

- vars:

  Numeric columns to summarise; default all numeric columns that are not
  in \`by\`.

- by:

  Grouping columns (character vector) or \`NULL\` for the whole data.

- stats:

  Statistics to return, any of \`"n"\`, \`"n_missing"\`,
  \`"pct_missing"\`, \`"mean"\`, \`"sd"\`, \`"median"\`, \`"q1"\`,
  \`"q3"\`, \`"iqr"\`, \`"min"\`, \`"max"\` (default all). \`n\` counts
  non-missing values; \`pct_missing\` is on a 0 to 100 scale.

## Value

A tibble with the \`by\` columns, \`variable\` and the requested
statistics, one row per group and variable (a missing \`by\` value forms
its own group). Statistics that cannot be computed (an all-missing
group, the SD of one value) are \`NA\`.

## See also

\[summary_table()\], \[tbl_table_one()\], \[describe_columns()\]

## Examples

``` r
calc_grouped_summary(mtcars, vars = c("mpg", "hp"), by = "cyl")
#> # A tibble: 6 × 13
#>     cyl variable     n n_missing pct_missing  mean    sd median    q1    q3
#>   <dbl> <chr>    <int>     <int>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1     4 mpg         11         0           0  26.7  4.51   26    22.8  30.4
#> 2     6 mpg          7         0           0  19.7  1.45   19.7  18.6  21  
#> 3     8 mpg         14         0           0  15.1  2.56   15.2  14.4  16.2
#> 4     4 hp          11         0           0  82.6 20.9    91    65.5  96  
#> 5     6 hp           7         0           0 122.  24.3   110   110   123  
#> 6     8 hp          14         0           0 209.  51.0   192.  176.  241. 
#> # ℹ 3 more variables: iqr <dbl>, min <dbl>, max <dbl>
calc_grouped_summary(airquality, stats = c("n", "pct_missing", "median"))
#> # A tibble: 6 × 4
#>   variable     n pct_missing median
#>   <chr>    <int>       <dbl>  <dbl>
#> 1 Ozone      116       24.2    31.5
#> 2 Solar.R    146        4.58  205  
#> 3 Wind       153        0       9.7
#> 4 Temp       153        0      79  
#> 5 Month      153        0       7  
#> 6 Day        153        0      16  
```
