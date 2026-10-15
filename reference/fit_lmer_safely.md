# Fit a Linear Mixed-Effects Model With Safe Fallback

Fits a linear mixed model via \[lme4::lmer()\], catching
non-convergence, singular fit warnings, or missing package conditions
and gracefully falling back to a fixed-effects linear model
(\[stats::lm()\]).

## Usage

``` r
fit_lmer_safely(formula, data, warn_on_fallback = TRUE, ...)
```

## Arguments

- formula:

  Model formula, typically containing random effects (e.g. \`y ~ x + (1
  \| id)\`).

- data:

  Data frame containing variables in \`formula\`.

- warn_on_fallback:

  Logical. Emit a warning when falling back to \`lm\`. Default \`TRUE\`.

- ...:

  Additional arguments passed to \[lme4::lmer()\] or \[stats::lm()\].

## Value

A list of class \`"cbe_lmer_safely"\` containing:

- model:

  The fitted \`merMod\` or \`lm\` object

- method:

  Character indicating \`"lmer"\` or \`"lm"\`

- is_singular:

  Logical indicating whether the mixed model had a singular fit

- formula:

  The formula used for the final fit

## Details

Replaces inline tryCatch / fallback copies across longitudinal reports.

## Examples

``` r
df <- data.frame(
  id = rep(1:5, each = 4),
  time = rep(1:4, times = 5),
  y = rnorm(20)
)
fit <- fit_lmer_safely(y ~ time + (1 | id), data = df)
#> boundary (singular) fit: see help('isSingular')
#> Warning: `lme4::lmer()` produced a singular fit; falling back to `stats::lm()`.
fit$method
#> [1] "lm"
```
