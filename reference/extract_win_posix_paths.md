# Extract Full \`.xlsx\` Paths From Text

Finds the first full Windows or POSIX path ending in an \`.xlsx\` file
name in each string, and splits it into folder and file name.

## Usage

``` r
extract_win_posix_paths(x, xlsx_token_re = xlsx_name_re)
```

## Arguments

- x:

  Character vector, e.g. code lines.

- xlsx_token_re:

  Regular expression for the file-name part. The default matches a name
  ending in \`.xlsx\` that may contain dots and spaces but not path
  separators, quotes, backticks, parentheses, commas, or \`=\`.

## Value

A tibble with one row per element of \`x\`: \`dir\`, \`file\`, and
\`full_path\` (\`NA\` where no path was found).

## See also

\[extract_all_xlsx_tokens()\]

## Examples

``` r
extract_win_posix_paths(c("C:\\data\\table_1.xlsx", "/home/me/out/table-2.xlsx", "none"))
#> # A tibble: 3 × 3
#>   dir            file         full_path                
#>   <chr>          <chr>        <chr>                    
#> 1 "C:\\data"     table_1.xlsx C:/data/table_1.xlsx     
#> 2 "/home/me/out" table-2.xlsx /home/me/out/table-2.xlsx
#> 3  NA            NA           NA                       
```
