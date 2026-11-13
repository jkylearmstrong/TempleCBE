# Cut Counting-Process Data at a Time Window

Restricts start/stop data to \`(0, at\]\`: intervals that start at or
after \`at\` are dropped, and an interval that runs past \`at\` is cut
there and becomes censored (\`status = 0\`), because its event, if any,
falls outside the window. Use it for 30-, 60- and 90-day readmission
windows; \[recurrent_cox_windows()\] does so for several windows at
once.

## Usage

``` r
truncate_start_stop(
  data,
  at,
  start = "tstart",
  stop = "tstop",
  status = "status"
)
```

## Arguments

- data:

  A data frame of counting-process intervals.

- at:

  The window length, in the units of the time columns.

- start, stop, status:

  Column names (strings). A factor \`status\` must have a level that
  means censored (\`"0"\`, \`"FALSE"\` or a label starting with
  "censor"); otherwise an interval that is cut would have to be given a
  guessed status, and the function stops.

## Value

\`data\` restricted to the window, with the same columns.

## See also

\[recurrent_cox_windows()\], \[recurrent_intervals()\]

## Examples

``` r
d <- data.frame(id = c(1, 1, 2), tstart = c(0, 20, 0), tstop = c(20, 70, 50), status = c(1, 1, 0))
truncate_start_stop(d, 30)
#>   id tstart tstop status
#> 1  1      0    20      1
#> 2  1     20    30      0
#> 3  2      0    30      0
```
