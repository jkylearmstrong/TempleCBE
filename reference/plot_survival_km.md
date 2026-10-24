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
  color = "#9D2235",
  title = "Kaplan-Meier Survival Estimate",
  caption = NULL,
  base_size = 13,
  risk_table = FALSE,
  p_value = FALSE,
  pval = p_value
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

  Primary line/ribbon color (default: Temple Cherry `"#9D2235"`).

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

## Value

A ggplot2 object, or (with \`risk_table = TRUE\`) a \`ggsurvfit\` plot
that draws the risk table under the curve when printed.
