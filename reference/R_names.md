# Clean Column Names, Preserving Originals as Labels

Runs
[`clean_names`](https://sfirke.github.io/janitor/reference/clean_names.html)
on `df` and stores each column's original name as a labelled variable
label, so the human-readable original isn't lost when the column name
itself becomes machine-friendly.

## Usage

``` r
R_names(df)
```

## Arguments

- df:

  A data frame or tibble.

## Value

A tibble with cleaned column names and the original names stored as
variable labels (see
[`var_label`](https://larmarange.github.io/labelled/reference/var_label.html)).

## Examples

``` r
df <- tibble::tibble(
  `name with spaces` = 1:10,
  `special * characters` = LETTERS[1:10]
)
R_names(df)
#> # A tibble: 10 × 2
#>    name_with_spaces special_characters
#>               <int> <chr>             
#>  1                1 A                 
#>  2                2 B                 
#>  3                3 C                 
#>  4                4 D                 
#>  5                5 E                 
#>  6                6 F                 
#>  7                7 G                 
#>  8                8 H                 
#>  9                9 I                 
#> 10               10 J                 
```
