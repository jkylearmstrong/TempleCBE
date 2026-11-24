# Mixed Model of the Integrated Brier Score on Alpha and Lambda

Fits \`IBS ~ alpha \* log10(lambda) + (1 \| resample)\` to a tuning grid
with \[lme4::lmer()\] and returns the fixed effects as a table, so the
report does not have to rebuild the model and its \`gtsummary\` call in
every chapter.

## Usage

``` r
tbl_tune_mixed_model(
  data,
  log_ibs = FALSE,
  random = NULL,
  interaction = TRUE,
  conf_level = 0.95,
  format = c("tibble", "gtsummary")
)
```

## Arguments

- data:

  A tuning table with \`alpha\`, \`lambda\`, \`IBS\` and a resample
  column (\`inner_resamples_splits\`, \`inner_resamples\` or \`split\`).
  Rows are reduced to one per model.

- log_ibs:

  If \`TRUE\`, the response is \`log10(IBS)\`. Default \`FALSE\`.

- random:

  Optional names of further grouping columns that get a random intercept
  each (for example the outer fold), in addition to the resample column.

- interaction:

  If \`TRUE\` (default) the model has the \`alpha\` by \`log10(lambda)\`
  interaction.

- conf_level:

  Confidence level (default 0.95).

- format:

  \`"tibble"\` (default) or \`"gtsummary"\`, a
  \[gtsummary::tbl_regression()\] table (needs the broom.helpers
  package, and broom.mixed for a mixed model).

## Value

A tibble with \`term\`, \`estimate\`, \`std.error\`, \`statistic\`,
\`conf.low\`, \`conf.high\` and \`p.value\`, with the fitted model in
\`attr(, "model")\`, the formula in \`attr(, "formula")\` and \`attr(,
"method")\` (\`"lmer"\` or \`"lm"\`); or a \`gtsummary\` table.

## Inference

The tibble reports Wald intervals and p-values from the normal
approximation (\`estimate / std.error\`) for the mixed model, which is
reasonable here because a tuning grid has many models per resample; they
are optimistic when there are few resamples. The models of one grid are
fitted to the same data, so treat the p-values as a description of the
response surface, not as tests. When there is a single resample, lme4 is
not installed, or the mixed model fails, an ordinary linear model is
fitted instead and \`attr(, "method")\` is \`"lm"\`; for that fallback
the intervals and p-values use the t-distribution with the model's
residual degrees of freedom, which is the correct small-sample reference
distribution for \`lm\`.

## See also

\[fit_ibs_mixed_model()\], \[tbl_tune_alpha()\], \[plot_tune_grid()\]

## Examples

``` r
set.seed(1)
tune <- expand.grid(split = paste0("Fold", 1:4), alpha = c(0.1, 0.5, 1),
                    lambda = c(0.01, 0.05, 0.1))
tune$IBS <- 0.15 + 0.03 * tune$alpha + 0.1 * tune$lambda + stats::rnorm(nrow(tune), 0, 0.01)
tbl_tune_mixed_model(tune)
#> # A tibble: 4 × 7
#>   term               estimate std.error statistic conf.low conf.high  p.value
#>   <chr>                 <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>
#> 1 (Intercept)         0.166     0.00974    17.0    0.147      0.185  3.54e-65
#> 2 alpha               0.0237    0.0150      1.58  -0.00571    0.0532 1.14e- 1
#> 3 log10_lambda        0.00793   0.00652     1.22  -0.00485    0.0207 2.24e- 1
#> 4 alpha:log10_lambda -0.00605   0.0101     -0.601 -0.0258     0.0137 5.48e- 1
```
