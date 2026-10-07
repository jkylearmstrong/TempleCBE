# Pattern Matching and Logical-Negation Infix Operators

A small family of [`grepl()`](https://rdrr.io/r/base/grep.html)/`%in%`
wrappers as base functions and matching infix operators.

## Usage

``` r
like(vector, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE)

vector %like% pattern

ilike(vector, pattern)

vector %ilike% pattern

flike(vector, pattern)

vector %flike% pattern

plike(vector, pattern)

vector %plike% pattern

notin(x, table)

x %!in% table

x %notin% table
```

## Arguments

- pattern:

  Pattern to match (for the `like` family).

- ignore.case, fixed, perl:

  Logical flags controlling how `pattern` is matched, as in
  [`grepl`](https://rdrr.io/r/base/grep.html).

- x, vector:

  A vector to test.

- table:

  Vector of values to test against (for `notin()`/`%notin%`/`%!in%`).

## Value

A logical vector.
