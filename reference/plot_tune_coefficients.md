# Coefficient Estimates Against the Integrated Brier Score

Scatter of each feature's penalized coefficient against the IBS of the
model it came from, over the whole tuning grid and, in a second panel,
over the best-performing share of the grid (lowest IBS). Coefficients
that stay away from zero among the best models are the stable ones.

## Usage

``` r
plot_tune_coefficients(
  data,
  top_frac = 0.2,
  terms = NULL,
  drop_zero = FALSE,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- data:

  A tuning table with one row per coefficient and model: \`term\`,
  \`estimate\` and \`IBS\` (as returned by \[tune_alpha_by_split()\] /
  \`glmnet_IBS()\` summaries).

- top_frac:

  Share of the models, by lowest IBS, shown in the second panel (default
  \`0.2\`, the top quintile). The cut is the \`top_frac\` quantile of
  IBS over distinct models, and models at the cut are included. \`NULL\`
  shows the full grid only.

- terms:

  Optional character vector of terms to show (raw names, before
  \`label_map\`).

- drop_zero:

  If \`TRUE\`, coefficients that are exactly zero (not selected) are
  left out. Default \`FALSE\`.

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.

## See also

\[plot_tune_grid()\], \[plot_tune_coef_boxplot()\], \[tbl_tune_coef()\]

## Examples

``` r
set.seed(1)
grid <- expand.grid(split = paste0("Fold", 1:3), alpha = c(0.1, 0.5, 1), lambda = c(0.01, 0.1))
grid$IBS <- 0.15 + 0.03 * grid$alpha + stats::rnorm(nrow(grid), 0, 0.01)
tune <- merge(grid, data.frame(term = c("age", "bmi")))
tune$estimate <- stats::rnorm(nrow(tune), 0.3, 0.1)
plot_tune_coefficients(tune)
```
