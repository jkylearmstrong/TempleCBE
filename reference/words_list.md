# Oxford-Comma List With a Fallback for Empty Input

Joins a vector into prose ("A", "A and B", "A, B, and C"). Unlike
\`knitr::combine_words()\`, an empty input returns \`none\` rather than
\`""\`, so narrative text never ends up with a hole in it.

## Usage

``` r
words_list(x, none = "none", and = "and")
```

## Arguments

- x:

  A vector (converted to character). \`NA\` entries are dropped.

- none:

  Text returned when nothing is left.

- and:

  The joining word (default \`"and"\`; use \`"or"\` for alternatives).
  Surrounding spaces are ignored.

## Value

A single string.

## See also

\[words()\], \[pluralize()\]

## Examples

``` r
words_list(c("age", "sex", "BMI"))
#> [1] "age, sex, and BMI"
words_list(character())
#> [1] "none"
words_list(c("a", "b"), and = "or")
#> [1] "a or b"
```
