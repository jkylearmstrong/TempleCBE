# Calculate Percentage of Missing Data Per Feature

Computes the count and percentage of missing and complete values for
each column in a dataset.

## Usage

``` r
features_percent_miss(data, na_list = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- na_list:

  Optional vector of additional values to treat as missing, passed to
  [`SumNa`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md)
  (beyond actual `NA`).

## Value

A tibble with columns \`feature\`, \`SumNa\`, \`SumComp\`, \`PctNa\`,
\`PctComp\` sorted descending by \`PctNa\`.

## Examples

``` r
df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
features_percent_miss(df)
#> # A tibble: 2 × 5
#>   feature SumNa SumComp PctNa PctComp
#>   <chr>   <int>   <int> <dbl>   <dbl>
#> 1 b           2       2  0.5     0.5 
#> 2 a           1       3  0.25    0.75

df2 <- data.frame(a = c(1, "NA", 3), b = c("", 2, 3))
features_percent_miss(df2, na_list = c("NA", ""))
#> # A tibble: 2 × 5
#>   feature SumNa SumComp PctNa PctComp
#>   <chr>   <int>   <int> <dbl>   <dbl>
#> 1 a           1       2 0.333   0.667
#> 2 b           1       2 0.333   0.667
```
