# Calculate Inner and Outer IQR Fences

Calculate Inner and Outer IQR Fences

## Usage

``` r
calculate_fences(col)
```

## Arguments

- col:

  A numeric vector or column.

## Value

A one-row tibble with columns `lower_inner_fence`, `upper_inner_fence`
(at 1.5 x IQR — conventional "mild" outlier boundary) and
`lower_outer_fence`, `upper_outer_fence` (at 3 x IQR — "extreme" outlier
boundary).

## Examples

``` r
calculate_fences(c(1, 2, 3, 4, 5, 100))
#> # A tibble: 1 × 4
#>   lower_inner_fence upper_inner_fence lower_outer_fence upper_outer_fence
#>               <dbl>             <dbl>             <dbl>             <dbl>
#> 1              -1.5               8.5             -5.25              12.2
```
