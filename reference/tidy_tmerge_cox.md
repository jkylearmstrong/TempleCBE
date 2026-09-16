# Tidy Construction of Counting-Process (Start-Stop) Survival Data

Merges repeated longitudinal measurement data with event/censoring data
(and optional baseline covariates) to build tidy start-stop survival
intervals (`tstart`, `tstop`, `event`, `event_label`). This provides a
tidy, pipe-friendly alternative to SAS DATA-step macros and
[`survival::tmerge`](https://rdrr.io/pkg/survival/man/tmerge.html) for
creating counting-process datasets for time-dependent Cox proportional
hazards models.

## Usage

``` r
tidy_tmerge_cox(
  measure_df,
  event_df,
  id = "subject_id",
  measure_time = "time",
  event_time = "event_time",
  event_type = "event_type",
  baseline_df = NULL,
  post_event = c("exclude", "include")
)
```

## Arguments

- measure_df:

  A data frame containing repeated longitudinal measurements.

- event_df:

  A data frame containing subject event or censoring times and event
  types.

- id:

  Column name identifying subjects across data frames (default:
  `"subject_id"`).

- measure_time:

  Column name in `measure_df` indicating measurement observation times
  (default: `"time"`).

- event_time:

  Column name in `event_df` indicating event or censoring time (default:
  `"event_time"`).

- event_type:

  Column name in `event_df` indicating event label or description
  (default: `"event_type"`).

- baseline_df:

  Optional data frame containing time-fixed baseline covariates keyed by
  `id`.

- post_event:

  Character string: `"exclude"` (default) drops measurements occurring
  after the subject's event time; `"include"` extends `event_time` to
  the maximum observed measurement time.

## Value

A tibble with columns `tstart`, `tstop`, `event` (1 if event occurred at
`tstop`, 0 otherwise), `event_label`, and all merged measurement and
baseline covariates.

## See also

\[cbe_cox_multi()\]
