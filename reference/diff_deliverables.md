# Compare Two Deliverable Bundles and Diff DOCX Reports

Compares two deliverable \`.zip\` archives (or directories) produced by
\[package_deliverables()\], identifying added, removed, unchanged, and
modified files. For Word (\`.docx\`) and text/HTML/Markdown reports,
performs a paragraph/line-level text diff so review notes can state
which reports moved and what text changed.

## Usage

``` r
diff_deliverables(old_zip, new_zip)
```

## Arguments

- old_zip:

  Path to the baseline \`.zip\` deliverable archive (or directory).

- new_zip:

  Path to the updated \`.zip\` deliverable archive (or directory).

## Value

A tibble of class \`cbe_deliverable_diff\` with columns: \`file\`
(relative path in the bundle), \`status\` (\`"unchanged"\`,
\`"modified"\`, \`"added"\`, or \`"removed"\`), \`hash_changed\`
(logical), \`text_changed\` (logical), \`n_added\` (integer count of
added paragraphs/lines), \`n_removed\` (integer count of removed
paragraphs/lines), \`diff_summary\` (concise text summary of changes),
and \`diff_lines\` (list column of \`-\`/\`+\` diff lines).

## See also

\[package_deliverables()\]
