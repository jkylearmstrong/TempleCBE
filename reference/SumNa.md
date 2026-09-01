# Count Total Missing (NA) Values

Calculates the total number of missing values across a vector, matrix,
or data frame, optionally treating additional values (e.g. `"NA"` as a
literal string, or `""`) as missing too.

## Usage

``` r
SumNa(x, na_list = NULL)
```

## Arguments

- x:

  A vector, matrix, or data frame.

- na_list:

  Optional vector of additional values to treat as missing, beyond
  actual `NA`.

## Value

An integer representing the total count of missing values.

## Examples

``` r
SumNa(c(1, 2, NA, 4, NA))
#> [1] 2
SumNa(data.frame(a = c(1, NA), b = c(NA, 2)))
#> [1] 2
SumNa(c(1, NA, "NA", 4), na_list = "NA")
#> [1] 2
```
