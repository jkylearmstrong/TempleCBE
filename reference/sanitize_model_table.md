# Clean a Mixed-Model, emmeans or ANOVA Table for Word and PDF Output

Fixes rendering artefacts that recur when \`broom.mixed\`, \`emmeans\`
and \`anova\` tables are sent to Word or PDF:

## Usage

``` r
sanitize_model_table(
  x,
  digits = 1,
  min_decimals = 4,
  model = NULL,
  dropped = NULL,
  rename_df = TRUE
)
```

## Arguments

- x:

  A data frame (tibble, \`summary_emm\`, \`anova\` table), or a
  character vector.

- digits:

  Decimals kept for conditioning values (default 1).

- min_decimals:

  Smallest number of decimals that marks a value as unrounded (default
  4).

- model:

  Optional fitted model (\`lm\`, \`glm\`, \`merMod\`) to look for
  coefficients dropped because of rank deficiency.

- dropped:

  Optional character vector of dropped coefficient names, instead of
  \`model\`.

- rename_df:

  If \`TRUE\` (default), rename the degrees-of-freedom and ratio
  columns.

## Value

\`x\` with the fixes applied. For a data frame, \`attr(, "footnotes")\`
is a character vector of footnotes to print under the table (empty when
none apply).

## Details

\* HTML-escaped pipes in random-effect terms (\`(1 &#124; subject_id)\`,
\`&124;\`, \`&vert;\`) become \`\|\`. \* Unrounded conditioning values
written into labels and notes (\`time40.4573170731707\`, \`time =
40.4573170731707\`) become \`time = 40.5\`. \* The degrees-of-freedom
columns \`NumDF\` and \`DenDF\` are renamed \`Num DF\` and \`Den DF\`
(and \`t.ratio\`, \`z.ratio\`, \`F.ratio\` to \`t ratio\`, \`z ratio\`,
\`F ratio\`), with a footnote defining them. \* Coefficients dropped for
rank deficiency get an explanatory footnote.

Text is changed in the column names, in character and factor columns,
and in the \`mesg\` attribute that \`emmeans\` summaries carry. Numeric
columns are not touched.

## Rounding rule

A number is rounded only when it has at least \`min_decimals\` decimals
and directly follows a name (letters, digits, \`\_\`, \`.\`, starting
with a letter), with or without \`=\`. A name that ends in digits and is
glued to its value without \`=\` cannot be told apart from the value
(\`x12.34567\` is read as \`x = 12.3\`); write such labels with \`=\`.

## See also

\[format_contrast_labels()\], \[fit_lmer_safely()\]

## Examples

``` r
tab <- data.frame(
  term = c("(Intercept)", "sd__(Intercept) (1 &#124; subject_id)"),
  contrast = c("time40.4573170731707", "group A - group B"),
  NumDF = c(1, 2), DenDF = c(38.2, 40.1)
)
out <- sanitize_model_table(tab)
out
#>                               term          contrast Num DF Den DF
#> 1                      (Intercept)       time = 40.5      1   38.2
#> 2 sd__(Intercept) (1 | subject_id) group A - group B      2   40.1
attr(out, "footnotes")
#> [1] "Num DF: numerator degrees of freedom of the F test (number of parameters tested). Den DF: denominator degrees of freedom (estimated for a mixed model)."
```
