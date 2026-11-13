# Excel Column Letters

\`excel_col_letters()\` converts column positions to Excel column
letters (\`1\` is \`"A"\`, \`26\` is \`"Z"\`, \`27\` is \`"AA"\`,
\`703\` is \`"AAA"\`). \`rename_to_excel()\` names the columns of a data
frame by their position letters, for sheets with multi-row headers that
are easier to address by position (\`df\$C\`) until proper names are
assigned.

## Usage

``` r
excel_col_letters(n)

rename_to_excel(df)
```

## Arguments

- n:

  Positive whole numbers (column positions). \`NA\` gives \`NA\`.

- df:

  A data frame.

## Value

\`excel_col_letters()\`: a character vector as long as \`n\`.
\`rename_to_excel()\`: \`df\` with its columns renamed \`A\`, \`B\`,
...; the original names are kept in \`attr(, "original_names")\`.

## See also

\[read_positional_excel_headers()\], \[excel_colnames()\]

## Examples

``` r
excel_col_letters(c(1, 26, 27, 52, 703))
#> [1] "A"   "Z"   "AA"  "AZ"  "AAA"
names(rename_to_excel(data.frame(id = 1, `Blood gas` = 2, check.names = FALSE)))
#> [1] "A" "B"
```
