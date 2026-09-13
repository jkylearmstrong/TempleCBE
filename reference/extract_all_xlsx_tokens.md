# Extract Every \`.xlsx\` File Name From Text

Extract Every \`.xlsx\` File Name From Text

## Usage

``` r
extract_all_xlsx_tokens(x, xlsx_token_re = xlsx_name_re)
```

## Arguments

- x:

  Character vector, e.g. code lines.

- xlsx_token_re:

  Regular expression for the file-name part. The default matches a name
  ending in \`.xlsx\` that may contain dots and spaces but not path
  separators, quotes, backticks, parentheses, commas, or \`=\`.

## Value

A list the length of \`x\`, each element a character vector of the
\`.xlsx\` file names found in that string (empty when none).

## See also

\[extract_win_posix_paths()\]

## Examples

``` r
extract_all_xlsx_tokens(c("read_excel('out/a.xlsx'); write_xlsx(df, 'b c.xlsx')", "none"))
#> [[1]]
#> [1] "a.xlsx"   "b c.xlsx"
#> 
#> [[2]]
#> character(0)
#> 
```
