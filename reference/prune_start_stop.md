# Clean Counting-Process Intervals

Companion to \[validate_start_stop()\]: drops intervals with \`stop \<=
start\`, drops rows recorded after a subject's first event, and closes
tiny gaps or overlaps (smaller than \`tol\`) between a subject's
consecutive intervals by moving the later \`start\` to the earlier
\`stop\`. Rows with a missing id, time or status are dropped too. Gaps
and overlaps larger than \`tol\` are real and left alone; check them
with \[validate_start_stop()\].

## Usage

``` r
prune_start_stop(
  data,
  id = "id",
  start = "tstart",
  stop = "tstop",
  status = "status",
  tol = 1e-06
)
```

## Arguments

- data:

  A data frame of counting-process intervals.

- id, start, stop, status:

  Column names (strings). Status is 0/1, logical or a 0/1 factor.

- tol:

  Largest gap or overlap treated as rounding error (default \`1e-6\`).

## Value

\`data\` with the offending rows removed, sorted by \`id\`, \`start\`,
\`stop\`. The counts are in \`attr(x, "pruned")\`: \`missing\`,
\`nonpositive\`, \`after_event\` and \`gaps_closed\`.

## See also

\[validate_start_stop()\]

## Examples

``` r
df <- data.frame(id = c(1, 1, 1, 2), tstart = c(0, 5.0000001, 10, 0),
                 tstop = c(5, 10, 12, 0), status = c(0, 1, 0, 0))
prune_start_stop(df)
#>   id tstart tstop status
#> 1  1      0     5      0
#> 2  1      5    10      1
```
