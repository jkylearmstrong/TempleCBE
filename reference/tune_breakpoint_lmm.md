# Sweep Candidate Knots of a Piecewise-Linear Mixed Model

Fits \`y ~ t + pmax(t - k, 0) + (1 + t \| id)\` by maximum likelihood
for each candidate change point \`k\` and reports AIC, BIC and the
in-sample RMSE (observed minus the conditional fit). A knot whose model
fails to fit gets \`NA\` metrics and a \`status\`.

## Usage

``` r
tune_breakpoint_lmm(
  data,
  response,
  time_col = "time",
  id_col = "arl_number",
  knots = NULL,
  metric = c("RMSE", "AIC", "BIC")
)
```

## Arguments

- data:

  A data frame in long format.

- response:

  Name of the numeric response.

- time_col, id_col:

  Names of the time and subject columns.

- knots:

  Candidate change points. \`NULL\` uses the observed times strictly
  between the smallest and the largest.

- metric:

  Criterion that picks the selected knot (lowest wins): \`"RMSE"\`,
  \`"AIC"\` or \`"BIC"\`.

## Value

A tibble with \`knot\`, \`AIC\`, \`BIC\`, \`RMSE\`, \`singular\`,
\`status\` and \`selected\` (\`TRUE\` for the best knot by \`metric\`;
none if every fit failed).

## Details

Needs \`lme4\`. Rows with a missing response, time or id are not used.
Note that the RMSE favours flexible fits; prefer AIC or BIC when the
knot should generalise.

## See also

\[impute_piecewise_lmm()\]

## Examples

``` r
if (FALSE) { # \dontrun{
d <- lme4::sleepstudy
tune_breakpoint_lmm(d, "Reaction", "Days", "Subject", knots = 2:7, metric = "AIC")
} # }
```
