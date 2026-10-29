# Presentation-Ready Cox Coefficient Table

Formats the `table` element of a \[cox_single()\] or \[cox_multi()\]
result into a presentation table, adding a log(HR) column (if not
already present) and optional sorting and significance annotation.

## Usage

``` r
cbe_cox_table(
  x,
  sort = c("none", "magnitude", "pvalue"),
  significance = FALSE,
  label_map = NULL,
  style = NULL
)

cox_table(
  x,
  sort = c("none", "magnitude", "pvalue"),
  significance = FALSE,
  label_map = NULL,
  style = NULL
)

tbl_cox(
  x,
  sort = c("none", "magnitude", "pvalue"),
  significance = FALSE,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- x:

  A `cbe_cox` (from \[cox_single()\]) or `cbe_cox_multi` (from
  \[cox_multi()\]) object.

- sort:

  One of `"none"` (default; original model order), `"magnitude"` (sorts
  by `abs(log(HR))` descending; reference rows, which are always exactly
  0, sort last), or `"pvalue"` (sorts by p-value ascending; reference
  rows, which have no p-value, sort last).

- significance:

  Logical; if `TRUE`, appends a `sig` column of significance stars
  (three asterisks for p \< 0.001, two for p \< 0.01, one for p \< 0.05)
  and bolds the `p.value` text of significant rows.

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A tibble formatted for presentation, with columns `Variable`, `Level`,
`Role`, `HR`, `log(HR)`, `95% CI`, `p.value`, and (if
`significance = TRUE`) `sig`.

## See also

\[cox_single()\], \[cox_multi()\]
