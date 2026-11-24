# Count and Word With the Right Plural

\`pluralize(1, "model")\` is \`"1 model"\`, \`pluralize(3, "model")\` is
\`"3 models"\`. Vectorised over \`n\`.

## Usage

``` r
pluralize(n, singular, plural = NULL, big_mark = "")
```

## Arguments

- n:

  Number(s).

- singular:

  The singular word.

- plural:

  The plural word (default: \`singular\` plus "s").

- big_mark:

  Thousands separator for the number (default none).

## Value

Character vector.

## Details

Only \`n == 1\` is singular, so 0, 2, 1.5 and negative numbers take the
plural (\`"0 models"\`). The default plural adds "s"; give \`plural\`
for other words (\`"child"\`, \`"children"\`). \`NA\` gives \`NA\`.

## See also

\[words_list()\]

## Examples

``` r
pluralize(c(0, 1, 2), "patient")
#> [1] "0 patients" "1 patient"  "2 patients"
pluralize(2, "child", "children")
#> [1] "2 children"
```
