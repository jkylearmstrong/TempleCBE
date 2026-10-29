# Impute Missing Values with a Piecewise-Linear Mixed Model

Fits \`y ~ t + pmax(t - knot, 0) + (1 + t \| id)\` (REML) to the
observed rows and fills the missing responses with the model prediction,
using each subject's random effects when the subject has observations
and the population curve otherwise. A single imputation: it does not
propagate the imputation uncertainty.

## Usage

``` r
impute_piecewise_lmm(
  data,
  response,
  time_col = "time",
  id_col = "arl_number",
  knot = NULL,
  metric = c("AIC", "BIC", "RMSE")
)
```

## Arguments

- data:

  A data frame in long format.

- response:

  Name of the numeric response.

- time_col, id_col:

  Names of the time and subject columns.

- knot:

  Change point. \`NULL\` picks it with \[tune_breakpoint_lmm()\] using
  \`metric\`.

- metric:

  Criterion for the automatic knot.

## Value

\`data\` as a tibble with two added columns, \`\<response\>\_imputed\`
(the response with missing values filled) and
\`\<response\>\_was_imputed\` (\`TRUE\` for filled rows). Rows with a
missing time or id stay \`NA\`. The attribute \`knot\` holds the change
point used.

## See also

\[tune_breakpoint_lmm()\]

## Examples

``` r
if (FALSE) { # \dontrun{
d <- lme4::sleepstudy
d$Reaction[c(5, 25, 60)] <- NA
out <- impute_piecewise_lmm(d, "Reaction", "Days", "Subject", knot = 4)
} # }
```
