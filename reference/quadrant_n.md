# One Quadrant Count for Inline Prose

One Quadrant Count for Inline Prose

## Usage

``` r
quadrant_n(tbl_quad, quadrant, col)
```

## Arguments

- tbl_quad:

  Result of \[tbl_quadrant_distribution()\].

- quadrant:

  Quadrant label.

- col:

  A \`level\` of the table (for example \`"Died"\`) or \`"Total"\`. A
  level that occurs in more than one variable is an error.

## Value

The count as an integer (\`0\` if the level exists but not in that
quadrant).

## See also

\[tbl_quadrant_distribution()\]

## Examples

``` r
d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
quadrant_n(tbl_quadrant_distribution(d, strat_vars = "am"), "I", "Total")
#> [1] 1
```
