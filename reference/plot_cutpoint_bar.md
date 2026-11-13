# Compare Cutpoint Selection Criteria

\`plot_cutpoint_bar()\` draws one bar per selection criterion (median
split, maximal log-rank, first significant cutpoint at each level,
lowest p-value) of \[km_cutpoint_search()\] or
\[km_cutpoint_time_varying()\], with the cutpoint value written on the
bar. \`plot_cutpoint_scatter()\` places the same criteria at their
cutpoint (x) and \`-log10(p)\` (y). Give a named list of results to
compare several features, one panel each.

## Usage

``` r
plot_cutpoint_bar(
  x,
  metric = c("neg_log10_p", "hr", "cutpoint"),
  alpha = 0.05,
  digits = 3
)

plot_cutpoint_scatter(x, alpha = 0.05)
```

## Arguments

- x:

  A \`cbe_km_cutpoint\` object, a (named) list of them, or a data frame
  with \`criterion\`, \`cutpoint\` and \`p_value\` (and optionally
  \`feature\`, \`hr\`).

- metric:

  What the bar height is: \`"neg_log10_p"\` (default), \`"hr"\` (hazard
  ratio of high versus low, on a log axis) or \`"cutpoint"\`.

- alpha:

  Significance levels drawn as dotted reference lines when the y-axis is
  \`-log10(p)\` (default \`0.05\`; \`NULL\` for none).

- digits:

  Significant digits of the cutpoint labels.

## Value

A \`ggplot2\` object.

## See also

\[km_cutpoint_search()\], \[plot_km_cutpoints()\],
\[plot_km_cutpoint()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(120, 0.1), status = rbinom(120, 1, 0.7), marker = rnorm(120))
res <- km_cutpoint_search(d, "time", "status", "marker")
plot_cutpoint_bar(res)

plot_cutpoint_scatter(res)
```
