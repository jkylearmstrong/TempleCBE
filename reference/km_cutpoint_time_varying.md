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
  n_cuts = 30
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

## Value

A list with class \`"cbe_km_cutpoint"\` containing optimal cutpoint and
candidate statistics.
