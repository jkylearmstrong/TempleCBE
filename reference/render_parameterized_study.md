# Render a Whole Folder of Reports Once per Parameter Set

\[render_parameterized_pair()\] for a folder: every \`.qmd\`/\`.Rmd\`
file in \`source_dir\` is rendered once per entry of \`param_list\`, and
the outputs of entry \`i\` go to \`target_dirs\[i\]\`. This replaces
keeping a mirrored copy of the folder (for example \`compute_index/\`
and \`compute_index_impute/\`) that differs only in a few parameters.
The templates themselves are never copied or changed.

## Usage

``` r
render_parameterized_study(
  source_dir,
  target_dirs,
  param_list,
  pattern = "\\.(qmd|Rmd)$",
  recursive = FALSE,
  formats = "html",
  on_error = c("continue", "stop"),
  dry_run = FALSE,
  ...
)
```

## Arguments

- source_dir:

  Folder with the templates.

- target_dirs:

  Output folders, one per parameter set (created if missing). Unnamed or
  named.

- param_list:

  A list of named parameter lists, the same length as \`target_dirs\`.

- pattern:

  Regular expression for the files to render (default
  \`"\\(qmd\|Rmd)\$"\`, ignoring case).

- recursive:

  Also look in sub-folders (default \`FALSE\`).

- formats:

  Output formats (default \`"html"\`).

- on_error:

  \`"continue"\` (default; failures are reported in the result) or
  \`"stop"\`.

- dry_run:

  Plan only; nothing is rendered.

- ...:

  Further arguments for \[render_variants()\] (\`self_contained\`,
  \`check_params\`, \`renderer\`, \`quiet\`).

## Value

A tibble, one row per document, parameter set and format, with the
columns of \[render_variants()\] plus \`document\` and \`target_dir\`.

## Details

Files whose name starts with \`\_\` or \`.\` are skipped (Quarto
includes and children). A document that does not declare a parameter in
its YAML header fails (see \[render_variants()\]); with \`on_error =
"continue"\` the others are still rendered.

## See also

\[render_parameterized_pair()\], \[render_variants()\]

## Examples

``` r
if (FALSE) { # \dontrun{
render_parameterized_study(
  "analysis/compute_index",
  target_dirs = c("reports/compute_index", "reports/compute_index_impute"),
  param_list  = list(list(use_imputed = FALSE), list(use_imputed = TRUE))
)
} # }
```
