# Best Imputation Method per Column

Picks, for each variable, the method with the lowest relative squared
error.

## Usage

``` r
tbl_best_imputation_method(rse_df)
```

## Arguments

- rse_df:

  A \[compare_imputation_rse()\] result, or \[tune_impute_numeric()\]
  output (anything with \`variable\`, \`method\` and \`rse\`).

## Value

A tibble with one row per variable: \`variable\`, \`best_method\`,
\`rse\`, \`runner_up\`, \`runner_up_rse\`, \`beats_null\`,
\`n_methods\`.

## Details

Ties go to the method that comes first in alphabetical order; methods
with a missing RSE are never chosen (a variable where all are missing
gets \`NA\`). \`beats_null\` says whether the winner's RSE is below 1.

## Examples

``` r
rse <- data.frame(variable = c("x", "x", "y", "y"),
                  method = c("mean", "knn", "mean", "knn"), rse = c(1, 0.4, 0.9, 1.2))
tbl_best_imputation_method(rse)
#> # A tibble: 2 × 7
#>   variable best_method   rse runner_up runner_up_rse beats_null n_methods
#>   <chr>    <chr>       <dbl> <chr>             <dbl> <lgl>          <int>
#> 1 x        knn           0.4 mean                1   TRUE               2
#> 2 y        mean          0.9 knn                 1.2 TRUE               2
```
