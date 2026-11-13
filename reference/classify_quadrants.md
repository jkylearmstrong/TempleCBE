# Assign Observations to Bivariate Quadrants

Splits observations by a cut on each of two columns. With the default
labels, \`"I"\` is x and y both at or above their cuts, \`"II"\` x below
and y at or above, \`"III"\` both below and \`"IV"\` x at or above and y
below (the mathematical numbering). A value exactly on a cut belongs to
the "at or above" side.

## Usage

``` r
classify_quadrants(
  data,
  x_col,
  y_col,
  x_cut = 0,
  y_cut = 0,
  labels = c("I", "II", "III", "IV"),
  quadrant_col = "quadrant"
)
```

## Arguments

- data:

  A data frame.

- x_col, y_col:

  Names of the two numeric columns.

- x_cut, y_cut:

  Cut values (default 0).

- labels:

  Four labels in the order above.

- quadrant_col:

  Name of the new factor column. Rows with a missing x or y get \`NA\`.

## Value

\`data\` as a tibble with the added factor column (all four levels
kept).

## See also

\[tbl_quadrant_distribution()\], \[quadrant_n()\]

## Examples

``` r
d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
table(d$quadrant)
#> 
#>   I  II III  IV 
#>   1  13   2  16 
```
