# Label Model Terms of Categorical Predictors With Their Reference Level

Rewrites dummy-coded model terms such as \`head_shapeHemis\` (the naming
of \[stats::model.matrix()\]) or \`head_shape_Hemis\` (the naming of
\`recipes::step_dummy()\`) as \`"Head Shape: Hemis (relative to
Flathead)"\`, so a table or forest plot says what each coefficient is
compared with. Terms that are not contrasts of a known factor are
returned unchanged, or with their own label from \`labels\`.

## Usage

``` r
format_contrast_labels(
  terms,
  data = NULL,
  reference = NULL,
  labels = NULL,
  template = "{variable}: {level} (relative to {reference})"
)
```

## Arguments

- terms:

  Character vector of model term names.

- data:

  Optional data frame the model was fitted to; its categorical columns
  give the variables, levels and references.

- reference:

  Optional named list giving the levels of each variable with the
  reference first, e.g. \`list(head_shape = c("Flathead", "Hemis"))\`.
  Used instead of, or in addition to, \`data\` (an entry here wins over
  the column of the same name).

- labels:

  Optional named character vector of display names for variables,
  \`c(head_shape = "Head Shape")\`. Without it the variable's \`label\`
  attribute is used, then \[pretty_name()\].

- template:

  Text with the placeholders \`{variable}\`, \`{level}\` and
  \`{reference}\`. Default \`"{variable}: {level} (relative to
  {reference})"\`.

## Value

A character vector as long as \`terms\`.

## How terms are matched

For every factor, character or logical column of \`data\` (or entry of
\`reference\`), the candidate term names are the variable name followed
by each non-reference level, joined by nothing or by \`\_\` or \`.\`,
with the level either as it is or passed through \[make.names()\]. A
term must equal a candidate exactly, so \`age\` is never mistaken for a
level of \`ag\`. The reference is the first level of a factor, the first
value in sort order of a character column, and \`FALSE\` for a logical.
This is correct for treatment contrasts (the default in R and in
\`step_dummy()\`); it does not describe sum, Helmert or polynomial
contrasts. Interaction terms (\`a:b\`) are labelled part by part and
joined by \`" x "\`.

## See also

\[pretty_name()\], \[relevel_factor()\]

## Examples

``` r
d <- data.frame(head_shape = factor(c("Flathead", "Hemis", "Round")), age = c(40, 50, 60))
format_contrast_labels(c("head_shapeHemis", "head_shape_Round", "age"), data = d)
#> [1] "Head Shape: Hemis (relative to Flathead)"
#> [2] "Head Shape: Round (relative to Flathead)"
#> [3] "age"                                     
format_contrast_labels("orientation_Spine", reference = list(orientation = c("Chestwall", "Spine")))
#> [1] "Orientation: Spine (relative to Chestwall)"
```
