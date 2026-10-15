# Normalize File Paths Without Failing

A vectorized \[normalizePath()\] that never errors or warns: blank and
\`NA\` inputs become \`NA\`, and a path that cannot be normalized comes
back as it was. Paths need not exist. Separators are forward slashes on
every platform.

## Usage

``` r
normalize_safely(x)
```

## Arguments

- x:

  Character vector of paths.

## Value

A character vector of normalized paths, named by the inputs.

## Details

\*\*Note:\*\* \`normalize_safely()\` normalizes \*filesystem paths\*
using \[normalizePath()\]. It does \*\*not\*\* scale or normalize
numeric numbers; for min-max scaling of numeric vectors, see
\[min_max_norm()\].

## See also

\[scan_data_io()\], \[min_max_norm()\]

## Examples

``` r
normalize_safely(c("data/../data/file.csv", "", NA))
#>   data/../data/file.csv                    <NA>                    <NA> 
#> "data/../data/file.csv"                      NA                      NA 
```
