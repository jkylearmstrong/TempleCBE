# Extract Survival Outcome Components and Predictors

Helper utility that parses a survival formula into clean components
supporting both 2-parameter `Surv(time, status)` and 3-parameter
start/stop `Surv(tstart, tstop, status)` counting process structures.

## Usage

``` r
extract_surv_components(data, outcome, subject_id = NULL)
```

## Arguments

- data:

  A data frame.

- outcome:

  A survival formula or Surv expression.

- subject_id:

  Optional subject identifier column name.

## Value

A list with elements `surv_obj`, `time`, `start`, `status`, `type`,
`predictors`, `pred_names`, and `formula`.
