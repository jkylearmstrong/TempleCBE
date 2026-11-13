# Optimal Survival Cutpoint Search for Time-Varying Counting Process Data

Evaluates candidate biomarker cutpoints on start-stop survival data
(\`Surv(tstart, tstop, status)\`), clustering by subject ID to avoid
data leakage.

## Usage

``` r
km_cutpoint_time_varying(
  data,
  tstart_col,
  tstop_col,
  status_col,
  var_col,
  id_col,
  min_prop = 0.1,
  n_cuts = 30,
  cutpoints = c("quantiles", "midpoints"),
  alpha = c(0.1, 0.05, 0.001)
)
```

## Arguments

- data:

  Long-format data frame with counting process layout.

- tstart_col:

  Character name of interval start time.

- tstop_col:

  Character name of interval stop time.

- status_col:

  Character name of event status column.

- var_col:

  Character name of continuous biomarker column.

- id_col:

  Character name of subject ID column.

- min_prop:

  Minimum proportion of total records in each subgroup (default 0.10).

- n_cuts:

  Number of candidate cutpoints (default 30).

- cutpoints, alpha:

  As in \[km_cutpoint_search()\]. The criteria use the robust
  (clustered) Wald test of the Cox model in place of the log-rank test,
  and there is no \`maxstat\` criterion.

## Value

A list with class \`"cbe_km_cutpoint"\` containing optimal cutpoint,
candidate statistics and the \`criteria\` table described in
\[km_cutpoint_search()\].
