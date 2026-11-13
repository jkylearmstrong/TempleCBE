# Apply a Review Tracker Back to a Report as a Resolutions Appendix

Merges the resolutions of a completed review tracker into a report
(\`.qmd\`, \`.Rmd\` or \`.md\`) by appending a "Review resolutions"
appendix that records, per tracker item, what was done and where. It is
the apply-back step of the review workflow: \[docx_review_extract()\]
extracts comments, \[review_tracker()\] builds the tracker,
\[format_review_tracker()\] formats it for the response, and this
function carries the closed resolutions back into the deliverable.

## Usage

``` r
apply_review_tracker(tracker, report, output = NULL, overwrite = FALSE)

# S3 method for class 'review_apply'
print(x, ...)
```

## Arguments

- tracker:

  A \`cbe_review_tracker\` tibble, a data frame with the \`status\`,
  \`location\` and \`rationale\` columns, or a path to a \`.xlsx\` or
  \`.csv\` tracker file (read as in \[validate_review_tracker()\]).

- report:

  Path to the \`.qmd\`, \`.Rmd\` or \`.md\` report to append the
  resolutions appendix to.

- output:

  Path for the report with the appendix. Default: the report name with
  \`\_reviewed\` before the extension, beside the report.

- overwrite:

  Logical (default \`FALSE\`): allow \`output\` to be an existing file.
  Appending in place (\`output\` the same file as \`report\`) also needs
  \`overwrite = TRUE\`.

- x:

  The object to print (the result of the function).

- ...:

  Not used.

## Value

Invisibly, a list of class \`review_apply\` with \`output\` (the written
path), \`applied\` (the applied tracker rows), \`not_applied\` (a tibble
with \`item_id\`, \`file\`, \`status\` and \`reason\`), \`n_items\` and
\`n_applied\`.

## What it does and does not do

A tracker row is \*applied\* when it is closed (adopted/addressed or not
adopted) and records a concrete location or rationale — an adopted row
needs a location, a not-adopted row needs a rationale, as checked by
\[validate_review_tracker()\]. Applied rows become the appendix table.
Rows that are still open, and closed rows with nothing concrete
recorded, are not applied; they are listed under "Not applied" with the
reason, so nothing is silently dropped. The function does \*\*not\*\*
edit the source \`.docx\` the comments came from, does \*\*not\*\*
change the report body, and does \*\*not\*\* mark tracker rows closed:
the appendix is a record of the resolutions, and the edits the tracker
describes still have to be made (or verified) in the report by the
author. Run \[validate_review_tracker()\] first when the tracker must be
complete before anything is applied.

## See also

\[review_tracker()\], \[format_review_tracker()\],
\[validate_review_tracker()\]

## Examples

``` r
tracker <- data.frame(
  item_id = 1:3, file = "manuscript.docx", author = c("Reviewer 1", "Reviewer 2", "Reviewer 1"),
  comment = c("Define the cohort", "Report the sample size", "Check Table 2"),
  status = c("adopted", "not_adopted", "open"),
  location = c("Methods, page 4", "", ""),
  rationale = c("Added the inclusion criteria", "Outside the scope of the study", "")
)
report <- file.path(tempdir(), "report.qmd")
writeLines(c("# Results", "", "The cohort is described here."), report)
res <- apply_review_tracker(tracker, report)
res$n_applied
#> [1] 2
cat(readLines(res$output), sep = "\n")
#> # Results
#> 
#> The cohort is described here.
#> 
#> 
#> ## Review resolutions
#> 
#> Review resolutions applied from the supplied tracker on 2026-11-13: 2 of 3 tracker item(s) are recorded below. The remaining items are listed under “Not applied” and still need a decision. This appendix records what the tracker says was done and where; it does not change the report body and does not edit the source document the comments came from.
#> 
#> | item_id | file | author | comment | status | location | rationale |
#> |---|---|---|---|---|---|---|
#> | 1 | manuscript.docx | Reviewer 1 | Define the cohort | adopted | Methods, page 4 | Added the inclusion criteria |
#> | 2 | manuscript.docx | Reviewer 2 | Report the sample size | not_adopted |  | Outside the scope of the study |
#> 
#> ### Not applied
#> 
#> - Item 3 (manuscript.docx): still open: no resolution recorded.
#> 
```
