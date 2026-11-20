# Cumulative Incidence with Competing Risks

Aalen-Johansen estimate of the cumulative incidence of each cause at
chosen times, optionally by group. Unlike \`1 - Kaplan-Meier\` with the
other causes treated as censored, this does not overstate a cause's
incidence when competing events remove subjects from risk.

## Usage

``` r
tbl_cumulative_incidence(
  data,
  time_col,
  event_col,
  group_col = NULL,
  times = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame, one row per subject.

- time_col:

  Follow-up time.

- event_col:

  Event type: 0 (or the first level of a factor) for censored, then one
  code per cause.

- group_col:

  Optional grouping column.

- times:

  Times at which to report. Default: the quartiles of the observed event
  times.

- conf_level:

  Confidence level of the pointwise interval.

## Value

A tibble with \`group\` (when \`group_col\` is used), \`cause\`,
\`time\`, \`cif\`, \`std.error\`, \`conf.low\`, \`conf.high\`.

## Scope

This describes cumulative incidence. It does not fit a Fine-Gray
subdistribution hazard model; for that use \`cmprsk::crr()\` or
\`tidycmprsk\`.

## See also

\[tbl_landmark_cutpoints()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(200, 0.1), type = sample(0:2, 200, TRUE, prob = c(0.3, 0.4, 0.3)),
                arm = rep(c("A", "B"), 100))
tbl_cumulative_incidence(d, "time", "type", group_col = "arm", times = c(5, 10))
#> # A tibble: 8 × 7
#>   group cause  time   cif std.error conf.low conf.high
#>   <chr> <chr> <dbl> <dbl>     <dbl>    <dbl>     <dbl>
#> 1 A     1         5 0.193    0.0410   0.128      0.293
#> 2 A     1        10 0.304    0.0493   0.221      0.418
#> 3 A     2         5 0.105    0.0313   0.0581     0.188
#> 4 A     2        10 0.141    0.0363   0.0848     0.233
#> 5 B     1         5 0.116    0.0330   0.0667     0.203
#> 6 B     1        10 0.268    0.0471   0.190      0.378
#> 7 B     2         5 0.105    0.0314   0.0582     0.188
#> 8 B     2        10 0.212    0.0434   0.142      0.316
```
