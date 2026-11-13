# Four-Panel Overview of a Penalized Tuning Grid

One figure with (A) \`alpha\` against IBS, (B) \`log10(IBS)\` against
the coefficient estimates, (C) \`alpha\` against \`log10(lambda)\` and
(D) \`log10(IBS)\` against \`log10(lambda)\`, replacing the patchwork
block that was repeated in the nested cross-validation reports.

## Usage

``` r
plot_tune_grid(
  data,
  terms = NULL,
  title = NULL,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- data:

  A tuning table with \`alpha\`, \`lambda\`, \`IBS\`, \`term\` and
  \`estimate\` (one row per coefficient and model). Models with a
  missing or non-positive \`lambda\` or \`IBS\` are dropped, because the
  panels take logarithms.

- terms:

  Optional character vector of terms for panel B (raw names).

- title:

  Optional overall title.

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`patchwork\` object (requires the patchwork package).

## See also

\[plot_tune_coefficients()\], \[plot_tune_alpha_ibs()\],
\[plot_tune_lambda_ibs()\]

## Examples

``` r
if (requireNamespace("patchwork", quietly = TRUE)) {
  set.seed(1)
  grid <- expand.grid(split = paste0("Fold", 1:3), alpha = c(0.1, 0.5, 1), lambda = c(0.01, 0.1))
  grid$IBS <- 0.15 + 0.03 * grid$alpha + stats::rnorm(nrow(grid), 0, 0.01)
  tune <- merge(grid, data.frame(term = c("age", "bmi")))
  tune$estimate <- stats::rnorm(nrow(tune), 0.3, 0.1)
  plot_tune_grid(tune)
}
```
