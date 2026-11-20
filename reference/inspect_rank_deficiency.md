# Audit Rank Deficiency of a Model's Fixed-Effects Matrix

With sparse or unobserved factor cells, \`lme4::lmer()\` silently drops
redundant columns of the fixed-effects model matrix, and \`lm()\` /
\`glm()\` return \`NA\` coefficients. This reports which terms were
dropped.

## Usage

``` r
inspect_rank_deficiency(model, verbose = TRUE)
```

## Arguments

- model:

  A \`merMod\` object (\`lmer\`, \`glmer\`) or an \`lm\` / \`glm\`
  object.

- verbose:

  Emit a markdown callout with \[message()\] when the model is
  rank-deficient.

## Value

A list of class \`cbe_rank_deficiency\` with \`rank_deficient\`
(logical), \`n_columns\` (columns of the full model matrix), \`rank\`,
\`dropped\` (names of the dropped terms) and \`message\` (the markdown
callout text; \`NULL\` if full rank), returned invisibly.

## See also

\[lmer_safely()\]

## Examples

``` r
d <- data.frame(y = c(1, 2, 3, 5, 4, 6), a = 1:6, b = 2 * (1:6))
inspect_rank_deficiency(lm(y ~ a + b, d))$dropped
#> > **Rank-deficient model matrix.** 1 of 3 fixed-effect column(s) were dropped (rank 2): `b`. The cells that define them are empty or confounded with other terms, so their effects are not estimated.
#> [1] "b"
```
