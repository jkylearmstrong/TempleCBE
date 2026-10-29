# Baseline-Anchored Longitudinal Composite Index

Builds a composite score for every subject and time point. Each variable
is standardised with the \*\*baseline\*\* mean and SD (at
\`baseline_time\`, across subjects), multiplied by its polarity
(\`alpha\`, +1 if a higher value means worse/more, -1 if lower does),
and the score is the mean of the available standardised variables:
\`index = mean_i( alpha_i \* (x_i - mean0_i) / sd0_i )\`. At baseline
each variable has mean 0, so the index has mean 0 at baseline and a
change reads as standard deviations of the baseline distribution. For
one time point (cross-sectional data) use \[compute_composite_index()\].

## Usage

``` r
compute_longitudinal_composite_index(
  data,
  variables,
  index_map = NULL,
  id_col = "subject_id",
  time_col = "time",
  baseline_time = 0,
  index_name = "composite_index",
  min_vars = 1L
)
```

## Arguments

- data:

  A data frame in long format.

- variables:

  Numeric columns that contribute.

- index_map:

  Optional data frame with columns \`variable\` and \`alpha\` (+1 or
  -1); variables not listed get +1.

- id_col, time_col:

  Subject and time columns.

- baseline_time:

  Time that defines the baseline rows.

- index_name:

  Name of the new column.

- min_vars:

  Smallest number of non-missing standardised variables a row needs to
  get a score (default 1); otherwise \`NA\`.

## Value

\`data\` as a tibble with the index column added. The attribute
\`baseline\` has one row per variable (\`variable\`, \`alpha\`,
\`mean0\`, \`sd0\`, \`n0\`). A variable whose baseline SD is 0 or
undefined (fewer than two baseline values) is left out with a warning.

## See also

\[compute_composite_index()\]

## Examples

``` r
set.seed(1)
d <- expand.grid(id = 1:20, time = c(0, 5, 15))
d$a <- rnorm(60, 10 + d$time / 5, 2)
d$b <- rnorm(60, 50 - d$time, 5)
out <- compute_longitudinal_composite_index(d, c("a", "b"), data.frame(variable = "b", alpha = -1),
                                            id_col = "id")
tapply(out$composite_index, out$time, mean)
#>             0             5            15 
#> -2.733789e-16  6.333390e-01  2.215371e+00 
```
