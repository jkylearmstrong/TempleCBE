# Expand and Knit Per-Domain Child Templates With Failure Isolation (E4)

Loops over \`domains\`, expands a child Quarto (\`.qmd\`) or R Markdown
(\`.Rmd\`) template for each domain using \[knitr::knit_expand()\], and
evaluates each child in its own isolated environment via
\[knitr::knit_child()\]. If a domain fails (when \`on_error =
"continue"\`), the error is caught, logged in the returned status table,
and rendered as a structured warning callout so remaining domains
continue to compile cleanly.

## Usage

``` r
domain_children(
  domains,
  child,
  ...,
  envir = parent.frame(),
  on_error = c("continue", "stop"),
  quiet = TRUE
)
```

## Arguments

- domains:

  Character vector of domain names (or a named list of per-domain
  parameter lists).

- child:

  Path to a child \`.qmd\` / \`.Rmd\` file, or a character string
  containing inline child template text.

- ...:

  Additional template variables passed to \[knitr::knit_expand()\] and
  bound in each domain's child evaluation environment.

- envir:

  Parent environment for child evaluation (defaults to
  \[parent.frame()\]). Each domain executes in \`new.env(parent =
  envir)\` so variables created in one domain never leak into or
  overwrite another.

- on_error:

  Behavior when a domain child errors: \`"continue"\` (default; records
  the failure, emits a warning, and continues with remaining domains) or
  \`"stop"\` (stops immediately).

- quiet:

  Logical (default \`TRUE\`). Passed to \[knitr::knit_child()\].

## Value

A character object of class \`c("cbe_domain_children", "knit_asis",
"character")\` containing the combined knitted output across domains,
with attributes and \`\$\` accessors:

- \`status\`:

  Tibble with columns \`domain\`, \`status\` (\`"succeeded"\` or
  \`"failed"\`), \`error\`, \`duration_sec\`, and \`output\`.

- \`failed_domains\`:

  Character vector of domain names that failed.

- \`succeeded_domains\`:

  Character vector of domain names that succeeded.

## Details

Inside the child template, the current domain is available both as the
`{{domain}}` \[knitr::knit_expand()\] placeholder and as the \`domain\`
variable in the child evaluation environment (\`envir\`). Any additional
named arguments in \`...\` are passed to \[knitr::knit_expand()\] and
bound in the child's environment (if a named argument has the same
length as \`domains\` or is named by \`domains\`, each domain receives
its corresponding element).

## See also

\[repro_appendix()\], \[nested_survival_run()\], \[render_pipeline()\]
