# Summary Table Function

Summary Table Function

## Usage

``` r
my_summary_table(
  data,
  var,
  na.rm = FALSE,
  data.output = TRUE,
  table.output = FALSE
)
```

## Arguments

- data:

  a dataframe or tibble

- var:

  variable to summarize

- na.rm:

  should NA values be removed, TRUE or FALSE, default is FALSE

- data.output:

  should the data be output, TRUE or FALSE, default is TRUE

- table.output:

  should the table be output, TRUE or FALSE, default is FALSE

## Value

either a dataframe or readable table

## Examples

``` r
mtcars |> 
  dplyr::group_by(cyl) |> 
  my_summary_table(mpg)
#> # A tibble: 3 × 8
#>     cyl     N  Mean    SD   min   Max SD_Error variable
#>   <dbl> <int> <dbl> <dbl> <dbl> <dbl>    <dbl> <chr>   
#> 1     4    11  26.7  4.51  21.4  33.9    1.36  mpg     
#> 2     6     7  19.7  1.45  17.8  21.4    0.549 mpg     
#> 3     8    14  15.1  2.56  10.4  19.2    0.684 mpg     
  
mtcars |> 
  dplyr::group_by(cyl) |> 
  my_summary_table(mpg, table.output = TRUE)
#> # A tibble: 3 × 6
#>     cyl     N SD_Error variable `Mean (SD)`  Range       
#>   <dbl> <int>    <dbl> <chr>    <chr>        <chr>       
#> 1     4    11    1.36  mpg      26.66 (4.51) 21.4 to 33.9
#> 2     6     7    0.549 mpg      19.74 (1.45) 17.8 to 21.4
#> 3     8    14    0.684 mpg      15.1 (2.56)  10.4 to 19.2
```
