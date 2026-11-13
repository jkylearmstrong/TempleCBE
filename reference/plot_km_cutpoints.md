# Plot Cutpoint Search Profile

Displays the profile of log-rank statistics (-log10(p)) across evaluated
candidate cutpoints, highlighting the optimal threshold.

## Usage

``` r
plot_km_cutpoints(x, ..., alpha = c(0.1, 0.05, 0.001))
```

## Arguments

- x:

  An object of class \`"cbe_km_cutpoint"\` from
  \[km_cutpoint_search()\].

- ...:

  Additional arguments (unused).

- alpha:

  Significance levels drawn as labeled horizontal reference lines at
  \`-log10(alpha)\` (default \`c(0.1, 0.05, 0.001)\`; \`NULL\` for
  none). A level far above every point is not drawn, so it does not
  stretch the axis.

## Value

A \`ggplot2\` object.

## See also

\[km_cutpoint_search()\], \[plot_cutpoint_bar()\]
