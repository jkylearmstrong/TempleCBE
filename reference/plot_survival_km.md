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
  base_size = 13
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

## Value

A ggplot2 object.
