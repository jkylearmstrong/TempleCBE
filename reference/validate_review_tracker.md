# Validate a Review Response Tracker for Required Location and Rationale

Verifies that every closed (non-trivial) row in a review tracker records
a concrete document location (e.g. page, section, table, or figure) when
adopted/addressed, or a substantive rationale when not adopted. Bare
tokens such as \`"TRUE"\`, \`"FALSE"\`, \`"done"\`, \`"yes"\`, \`"no"\`,
or \`"ok"\` are rejected.

## Usage

``` r
validate_review_tracker(tracker, allow_open = FALSE, error_on_invalid = TRUE)
```

## Arguments

- tracker:

  A \`cbe_review_tracker\` tibble, data frame, or file path to a
  \`.xlsx\` or \`.csv\` review tracker.

- allow_open:

  Logical (default \`FALSE\`). When \`FALSE\`, rows with status
  \`"open"\` (or blank/\`NA\`) are also flagged as incomplete. When
  \`TRUE\`, only rows marked closed/resolved are checked for required
  location and rationale.

- error_on_invalid:

  Logical (default \`TRUE\`). When \`TRUE\`, stops with an informative
  error listing all invalid rows. When \`FALSE\`, returns a validation
  report object with \`\$valid = FALSE\` and \`\$issues\`.

## Value

A list of class \`cbe_review_tracker_validation\` with elements
\`valid\` (logical), \`n_rows\` (integer), \`n_invalid\` (integer), and
\`issues\` (a tibble with columns \`row\`, \`item_id\`, \`file\`,
\`status\`, and \`reason\`).

## See also

\[cbe_review_tracker()\], \[cbe_docx_review_extract()\]
