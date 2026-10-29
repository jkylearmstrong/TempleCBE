# Convert a Kaplan-Meier Summary Table to a GraphPad Prism Survival Table

Expands a summary-by-time table of event and censoring counts (as from
\`summary(survival::survfit(...))\`) into Prism's survival data layout:
one row per subject, an \`X\` column of times, and one column per group
holding 1 for an event, 0 for censored, and \`NA\` for the other groups.

## Usage

``` r
km_summary_to_prism(
  x,
  sheet = NULL,
  time_col = "time",
  strata_col = "strata",
  n_event_col = "n.event",
  n_censor_col = "n.censor",
  strata_levels = NULL,
  drop_time0 = TRUE,
  validate_totals = TRUE,
  out_xlsx = NULL,
  out_xlsx_paste = NULL
)
```

## Arguments

- x:

  A data frame, or a path to an \`.xlsx\`, \`.xls\`, \`.csv\`, or
  \`.txt\` file, with one row per time per group.

- sheet:

  Sheet name or index when \`x\` is an Excel file.

- time_col, strata_col, n_event_col, n_censor_col:

  Column names of the time, group, number of events, and number
  censored.

- strata_levels:

  Optional group order for the output columns. Every group in the data
  must be listed.

- drop_time0:

  Drop rows at time 0 (default \`TRUE\`).

- validate_totals:

  If \`TRUE\` (default) and \`x\` has an \`n.risk\` column, warn when a
  group's \`n.risk\` at its earliest time differs from its total events
  plus censored.

- out_xlsx:

  Optional path to write the table as Excel (requires \`writexl\`).

- out_xlsx_paste:

  Optional path to write a copy with blanks instead of \`NA\`,
  convenient for pasting into Prism.

## Value

A data frame with column \`X\` and one integer column per group, with a
\`"notes"\` attribute describing the layout.

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  s <- summary(fit, censored = TRUE)
  km <- data.frame(time = s$time, strata = s$strata, n.risk = s$n.risk,
                   n.event = s$n.event, n.censor = s$n.censor)
  head(km_summary_to_prism(km))
}
#>    X sex=1 sex=2
#> 1  5    NA     1
#> 2 11     1    NA
#> 3 11     1    NA
#> 4 11     1    NA
#> 5 12     1    NA
#> 6 13     1    NA
```
