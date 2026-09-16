# File Metadata as a Tibble

Vectorized file information from \[fs::file_info()\], with names and
parent folders split out for joining.

## Usage

``` r
file_meta_fs(paths)
```

## Arguments

- paths:

  Character vector of file paths.

## Value

A tibble with one row per path: \`path\`, \`file_name\`, \`dir_name\`
(parent folder name), \`dir_path\`, \`m_time\` (modified), \`c_time\`
(created, falling back to changed, then modified), \`size\` in bytes,
and \`uname\` (owner, where the platform reports one).

## See also

\[scan_data_io()\]

## Examples

``` r
f <- tempfile(fileext = ".csv")
writeLines("a,b", f)
file_meta_fs(f)
#> # A tibble: 1 × 8
#>   path       file_name dir_name dir_path m_time              c_time             
#>   <chr>      <chr>     <chr>    <chr>    <dttm>              <dttm>             
#> 1 /tmp/Rtmp… file1f3d… RtmphwL… /tmp/Rt… 2026-09-16 20:45:24 2026-09-16 20:45:24
#> # ℹ 2 more variables: size <dbl>, uname <chr>
```
