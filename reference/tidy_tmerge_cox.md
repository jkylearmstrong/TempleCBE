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
  post_event = c("exclude", "include"),
  censor_types = NULL
)
```

## Arguments

- measure_df:

  A data frame containing repeated longitudinal measurements.

- event_df:

  A data frame containing subject event or censoring times and event
  types. It must have one row per subject: repeated ids are an error,
  because the join would multiply the measurement rows. Subjects that
  are in `event_df` but have no measurement are not in the result.

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
  `id`. Like `event_df`, it must have one row per subject.

- post_event:

  Character string: what to do with measurements made at or after a
  subject's `event_time`.

  - `"exclude"` (default) drops them, so each subject's last interval
    ends at the recorded `event_time`.

  - `"include"` keeps the follow-up that the later measurements show:
    when a subject's last measurement is later than `event_time`,
    `event_time` is moved forward to that last measurement time, and the
    event (or censoring) happens there. The recorded `event_time` is not
    kept, so **the survival time of such a subject changes**: with
    measurements at 0, 3 and 10 and an `event_time` of 8, the intervals
    are (0, 3\] and (3, 10\], with `event = 1` on (3, 10\] only. The
    measurement at 10 gives that end point and no interval of its own,
    so the result never has a zero-length interval (10, 10\] or a second
    `event = 1` for the subject. See the examples.

- censor_types:

  Optional character vector of `event_type` values that mark a censoring
  time rather than an event, such as `"Censored"`. Those subjects' last
  interval gets `event = 0` (and no `event_label`). By default (`NULL`)
  every subject with an `event_time` has `event = 1` at it, whatever its
  `event_type`, so censoring times in `event_df` must be declared here
  (or the resulting `event` recoded) before the data is used to fit a
  survival model.

## Value

A tibble with columns `tstart`, `tstop`, `event` (1 if event occurred at
`tstop`, 0 otherwise), `event_label`, and all merged measurement and
baseline covariates.

Subjects can be missing from the result, or lose their last measurement,
and a warning names them: (1) subjects in `measure_df` without an
`event_time` in `event_df` (no row, or `NA`) have no end for their last
interval, so that measurement gets no interval, and a subject with a
single measurement drops out; (2) subjects with no row in the result at
all: with `post_event = "exclude"` because their `event_time` is at or
before their first measurement, with `"include"` because all their
measurements are at one time, at or after their `event_time`. A warning
also names the subjects with an interval whose `tstop` is not after its
`tstart`, which comes from repeated measurement times and which survival
models can't use.

## Details

Each measurement starts an interval that ends at the subject's next
measurement; the last interval ends at `event_time`, where `event` is 1
(unless the subject's `event_type` is in `censor_types`). A subject
therefore needs an `event_time`, and at least one measurement before it
(before the moved `event_time`, with `post_event = "include"`), to
appear in the result.

## See also

\[cbe_cox_multi()\]

## Examples

``` r
measures <- data.frame(
  subject_id = c(1, 1, 1),
  time = c(0, 3, 10),
  biomarker = c(1.2, 1.5, 1.8)
)
events <- data.frame(subject_id = 1, event_time = 8, event_type = "Relapse")

# "exclude": the measurement at 10 is after the event and dropped; the last
# interval ends at the event time, (3, 8].
tidy_tmerge_cox(measures, events)
#> # A tibble: 2 × 9
#>   subject_id  time biomarker event_time event_type tstart tstop event
#>        <dbl> <dbl>     <dbl>      <dbl> <chr>       <dbl> <dbl> <dbl>
#> 1          1     0       1.2          8 Relapse         0     3     0
#> 2          1     3       1.5          8 Relapse         3     8     1
#> # ℹ 1 more variable: event_label <chr>

# "include": the event time moves to the last measurement time, so the
# survival time is 10, not 8, and the last interval is (3, 10].
tidy_tmerge_cox(measures, events, post_event = "include")
#> # A tibble: 2 × 9
#>   subject_id  time biomarker event_time event_type tstart tstop event
#>        <dbl> <dbl>     <dbl>      <dbl> <chr>       <dbl> <dbl> <dbl>
#> 1          1     0       1.2         10 Relapse         0     3     0
#> 2          1     3       1.5         10 Relapse         3    10     1
#> # ℹ 1 more variable: event_label <chr>
```
