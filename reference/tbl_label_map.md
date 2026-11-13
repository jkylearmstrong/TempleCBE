# Format a Variable Label Map Table for Report Appendices

Produces a summary table listing raw variable names (\`term\`), short
figure labels (\`short\`), and full descriptive names (\`full\`) for
inclusion in a report legend or appendix.

## Usage

``` r
tbl_label_map(label_map = NULL, terms = NULL, format = c("tibble", "gt"))

cbe_label_table(label_map = NULL, terms = NULL, format = c("tibble", "gt"))
```

## Arguments

- label_map:

  Optional label map (anything accepted by \[cbe_labels()\]). Defaults
  to the active package-level map from \[cbe_labels()\].

- terms:

  Optional character vector of variable names to include (and order by).

- format:

  Output format: \`"tibble"\` (default) or \`"gt"\` (requires gt).

## Value

A tibble with columns \`term\`, \`short\`, and \`full\`, or a \`gt_tbl\`
object when \`format = "gt"\`.

## See also

\[cbe_labels()\]
