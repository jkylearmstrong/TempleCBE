# Recurrent-Event Cox Models Over 30-, 60- and 90-Day Windows

Cuts the counting-process data at each window with
\[truncate_start_stop()\] and fits \[recurrent_cox()\] to every cut, so
the effect on recurrent readmissions within 30, 60 and 90 days can be
read side by side.

## Usage

``` r
recurrent_cox_windows(
  data,
  predictors,
  windows = c(30, 60, 90),
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  ...
)
```

## Arguments

- data:

  A data frame of counting-process intervals.

- predictors:

  Covariates: a character vector of column names, or a one-sided formula
  such as \`~ age + sex\`.

- windows:

  Window lengths in the units of the time columns (default \`c(30, 60,
  90)\`).

- id, start, stop, status:

  Column names (strings) of the subject identifier, the interval start
  and stop, and the event indicator (0/1 or logical).

- ...:

  Passed to \[recurrent_cox()\] (\`model\`, \`max_events\`,
  \`conf_level\`, ...).

## Value

A tibble with one row per window and term: \`window\`, the columns of
the \`terms\` table of \[recurrent_cox()\], and \`n_events\` and
\`n_subjects\` for the window. The fits are in \`attr(x, "fits")\`, a
list named by window. A window without events is skipped with a warning.

## See also

\[recurrent_cox()\], \[truncate_start_stop()\]

## Examples

``` r
d <- data.frame(
  id = rep(1:40, each = 2), age = rep(seq(-1, 1, length.out = 40), each = 2),
  tstart = rep(c(0, 25), 40), tstop = rep(c(25, 80), 40),
  status = rep(c(1, 0), 40)
)
d$status[c(5, 6, 12, 30, 31, 40, 52, 70)] <- 1
recurrent_cox_windows(d, ~ age, windows = c(30, 60), model = "ag")
#> # A tibble: 2 × 11
#>   window term  estimate std_error    hr conf_low conf_high statistic p_value
#>    <dbl> <chr>    <dbl>     <dbl> <dbl>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1     30 age          0  7.92e-17     1        1         1         0       1
#> 2     60 age          0  7.92e-17     1        1         1         0       1
#> # ℹ 2 more variables: n_events <int>, n_subjects <int>
```
