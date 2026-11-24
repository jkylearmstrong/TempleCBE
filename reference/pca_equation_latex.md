# PCA Component as a LaTeX Linear Combination

Writes one principal component as a LaTeX sum of signed loadings times
feature names, for a report (for example PC_1 = +0.412 x - 0.231 y).
Loadings are the elements of \`rotation\` (signed; the sign of a
component is arbitrary). Underscores and other LaTeX specials in feature
names are escaped.

## Usage

``` r
pca_equation_latex(
  pca_mod,
  pc = 1,
  feature_names = NULL,
  max_terms = NULL,
  prefix = "PC",
  digits = 3
)
```

## Arguments

- pca_mod:

  A \[stats::prcomp()\] object or a \[run_pca()\] result.

- pc:

  Component number or name (\`"PC2"\`); exactly one.

- feature_names:

  Optional display names for the features, either one per rotation row
  (in order) or a named vector (names = original features) that may
  cover only some of them. \`NULL\` uses the row names of the rotation.

- max_terms:

  Keep only this many terms, those with the largest absolute loading
  (shown in that order, followed by \`...\`); \`NULL\` keeps all in the
  original order.

- prefix:

  Component label prefix.

- digits:

  Decimals of the loadings.

## Value

A single string of LaTeX math, without delimiters (wrap it in \`\$\$ ...
\$\$\`).

## See also

\[pca_component_cutoff()\], \[pca_top_features()\], \[pca_eqns()\]

## Examples

``` r
fit <- prcomp(mtcars[, 1:4], scale. = TRUE)
pca_equation_latex(fit, 1, digits = 2)
#> [1] "PC_{1} = -0.50 \\text{mpg} +0.51 \\text{cyl} +0.51 \\text{disp} +0.48 \\text{hp}"
pca_equation_latex(fit, 2, max_terms = 2)
#> [1] "PC_{2} = +0.848 \\text{hp} +0.415 \\text{mpg} \\dots"
```
