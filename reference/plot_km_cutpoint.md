# Kaplan-Meier Curves at a Selected Cutpoint

Splits the cohort of a \[km_cutpoint_search()\] (or
\[km_cutpoint_time_varying()\]) result into Low (\`\<= cutpoint\`) and
High (\`\> cutpoint\`) at the cutpoint of one selection criterion and
draws the two Kaplan-Meier curves with \[plot_survival_km()\], with the
cutpoint, hazard ratio and p-value in the subtitle.

## Usage

``` r
plot_km_cutpoint(
  x,
  criterion = "max_logrank",
  risk_table = TRUE,
  time_breaks = NULL,
  title = NULL,
  base_size = 13
)
```

## Arguments

- x:

  A \`cbe_km_cutpoint\` object.

- criterion:

  Name of a row of \`x\$criteria\` (default \`"max_logrank"\`, the
  optimum), or a number to use as the cutpoint directly.

- risk_table, time_breaks, base_size:

  Passed to \[plot_survival_km()\].

- title:

  Plot title (default: the variable name).

## Value

A \`ggplot2\` / \`ggsurvfit\` object.

## See also

\[km_cutpoint_search()\], \[plot_survival_km()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(120, 0.1), status = rbinom(120, 1, 0.7), marker = rnorm(120))
res <- km_cutpoint_search(d, "time", "status", "marker")
plot_km_cutpoint(res, criterion = "median")
```
