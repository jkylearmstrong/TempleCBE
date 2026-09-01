# Clean and Standardize Variable Names

Converts variable/column names to lower snake_case and removes invalid
special characters.

## Usage

``` r
clean_names(data)
```

## Arguments

- data:

  A data frame, tibble, or character vector of names.

## Value

Cleaned data frame or character vector.

## Examples

``` r
clean_names(c("First Name", "ZIP Code", "Total ($)"))
#> [1] "first_name" "zip_code"   "total"     
```
