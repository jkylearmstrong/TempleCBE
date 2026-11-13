# Render One Template Twice: the Original and the Imputed Analysis

Renders a single parameterized report for two parameter sets, typically
the analysis of the original data and of the imputed data, so that the
two reports come from one template instead of from two copies that drift
apart (a data-cleaning fix made in one and forgotten in the other). It
is \[render_variants()\] for exactly two variants, with two guards: \*
the two parameter sets must name the \*\*same parameters\*\*. A
parameter set in one and not in the other is nearly always a forgotten
difference, so it is an error before anything is rendered. (Set it in
both, to its default if it should not differ. \[render_variants()\]
itself allows variants that set different parameters.) \*
\`output_names\` must be two different, non-empty names.

## Usage

``` r
render_parameterized_pair(
  input,
  params_original,
  params_imputed,
  output_names = c("original", "imputed"),
  formats = "html",
  output_dir = NULL,
  ...
)
```

## Arguments

- input:

  Path to the \`.qmd\` or \`.Rmd\` template.

- params_original, params_imputed:

  Named lists of parameters for the two renderings. They must name the
  same parameters.

- output_names:

  Two different names for the two renderings, used in the output file
  names; letters, digits, \`\_\`, \`.\` and \`-\`. Default
  \`c("original", "imputed")\`.

- formats:

  Output formats for each rendering, e.g. \`c("html", "docx")\`.

- output_dir:

  Folder for the outputs.

- ...:

  Further arguments for \[render_variants()\] (\`stop_on_error\`,
  \`dry_run\`, \`self_contained\`, \`check_params\`, \`renderer\`,
  \`quiet\`).

## Value

The tibble of \[render_variants()\]: one row per rendering and format.

## Details

The template must declare the parameters in its YAML header under
\`params:\`; that is checked by \[render_variants()\]. Quarto passes
\`execute_params\` to documents that use the Knitr engine and read
\`params\$\<name\>\`; a template whose parameters live elsewhere (a
Jupyter notebook's \`parameters\` cell, say) needs \`check_params =
FALSE\`, passed on in \`...\`.

The outputs are named \`\<template\>\_\<output_names\[1\]\>.\<ext\>\`
and \`\<template\>\_\<output_names\[2\]\>.\<ext\>\`, written to
\`output_dir\` (the template's folder by default). Your own files are
not touched; see \[render_variants()\] for how the renderings are staged
and which files are replaced.

## See also

\[render_variants()\], \[render_pipeline()\]

## Examples

``` r
if (FALSE) { # \dontrun{
render_parameterized_pair(
  "analysis/mixed_models.qmd",
  params_original = list(data_suffix = "original", impute = FALSE),
  params_imputed  = list(data_suffix = "imputed",  impute = TRUE),
  output_names    = c("original", "imputed"),   # -> mixed_models_original.html, mixed_models_imputed.html
  formats         = c("html", "docx"),
  output_dir      = "reports"
)
} # }
```
