# Presentation-Ready Cox Coefficient Table

Formats the `table` element of a \[cbe_cox_single()\] or
\[cbe_cox_multi()\] result into a presentation table, adding a log(HR)
column (if not already present) and optional sorting and significance
annotation.

## Usage

``` r
cbe_cox_table(x, sort = c("none", "magnitude", "pvalue"), significance = FALSE)
```

## Arguments

- x:

  A `cbe_cox` (from \[cbe_cox_single()\]) or `cbe_cox_multi` (from
  \[cbe_cox_multi()\]) object.

- sort:

  One of `"none"` (default; original model order), `"magnitude"` (sorts
  by `abs(log(HR))` descending; reference rows, which are always exactly
  0, sort last), or `"pvalue"` (sorts by p-value ascending; reference
  rows, which have no p-value, sort last).

- significance:

  Logical; if `TRUE`, appends a `sig` column of significance stars
  (three asterisks for p \< 0.001, two for p \< 0.01, one for p \< 0.05)
  and bolds the `p.value` text of significant rows.

## Value

A tibble formatted for presentation, with columns `Variable`, `Level`,
`Role`, `HR`, `log(HR)`, `95% CI`, `p.value`, and (if
`significance = TRUE`) `sig`.

## See also

\[cbe_cox_single()\], \[cbe_cox_multi()\]
