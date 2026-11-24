# Standardized Kaplan-Meier Survival Curve

Renders a clean Kaplan-Meier curve using the Temple Cherry palette, with
censor marks, confidence ribbons, and percentage-formatted survival
axis.

## Usage

``` r
plot_survival_km(
  data,
  time_col,
  status_col,
  group_col = NULL,
  color = "#A41E35",
  title = "Kaplan-Meier Survival Estimate",
  caption = NULL,
  base_size = 13,
  risk_table = FALSE,
  p_value = FALSE,
  pval = p_value,
  time_breaks = NULL,
  start_col = NULL
)
```

## Arguments

- data:

  Data frame containing survival inputs.

- time_col:

  Character string naming the follow-up time column.

- status_col:

  Character string naming the event status column (1 = event, 0 =
  censored).

- group_col:

  Optional character string naming a stratification grouping column.

- color:

  Primary line/ribbon color (default: Temple Cherry `"#A41E35"`).

- title:

  Optional plot title.

- caption:

  Optional plot caption.

- base_size:

  Base font size (default: 13).

- risk_table:

  Logical. If \`TRUE\`, appends a number-at-risk table below the
  survival curve, with the same time axis and the same strata and colors
  as the curve (\[ggsurvfit::add_risktable()\]). Default \`FALSE\`.

- p_value:

  Logical. If \`TRUE\` and \`group_col\` is specified, displays the
  log-rank test p-value (the test of \[survival::survdiff()\], \`rho =
  0\`, comparing all strata at once) inside the plot. Default \`FALSE\`.
  Asking for it without a \`group_col\`, or with a group that has a
  single level, gives a warning and no p-value.

- pval:

  Same as \`p_value\`; the short name used by other survival plotting
  packages (\`survminer::ggsurvplot(pval = TRUE)\`). If both are given,
  \`pval\` wins.

- time_breaks:

  Optional numeric vector of follow-up times. They become the x-axis
  breaks and, with \`risk_table = TRUE\`, the labeled columns of the
  number-at-risk table. Default \`NULL\` lets \`ggsurvfit\` choose.

- start_col:

  Optional name of an interval start column for counting-process (start,
  stop\] data; \`time_col\` is then the interval stop time and the curve
  is fitted to \`Surv(start, stop, status)\`. With a \`group_col\` that
  changes within a subject, each interval counts in the group it belongs
  to (an extended Kaplan-Meier estimate), so the curves describe the
  hazard while in a group, not cohorts fixed at baseline. The log-rank
  test of \[survival::survdiff()\] does not accept such data; the
  p-value shown is the score (log-rank) test of the Cox model
  \`Surv(start, stop, status) ~ group\`, which treats intervals of one
  subject as independent given the group.

## Value

A ggplot2 object, or (with \`risk_table = TRUE\`) a \`ggsurvfit\` plot
that draws the risk table under the curve when printed.
