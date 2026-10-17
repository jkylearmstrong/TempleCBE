# Build a Structured Review Response Tracker Requiring Location and Rationale

Constructs a review response tracker table (and optional Excel workbook
or CSV) from the output of \[cbe_docx_review_extract()\]. Each comment
(and optionally each reconstructed redline) is given explicit
\`status\`, \`location\` (page, section, table, or figure where
addressed), and \`rationale\` (reason if not adopted, or summary of
changes made) columns for validation by \[validate_review_tracker()\].

## Usage

``` r
cbe_review_tracker(
  extract,
  file = NULL,
  include_redlines = FALSE,
  overwrite = TRUE
)
```

## Arguments

- extract:

  An object of class \`cbe_review_extract\` from
  \[cbe_docx_review_extract()\], a data frame of extracted comments, or
  a path to a \`.docx\` file or directory of \`.docx\` files.

- file:

  Optional output path (\`.xlsx\` or \`.csv\`) to write the tracker
  sheet.

- include_redlines:

  Logical (default \`FALSE\`). When \`TRUE\` and \`extract\` is a
  \`cbe_review_extract\` object with non-empty \`\$redlines\`, includes
  tracked paragraph redlines in addition to comments.

- overwrite:

  Logical (default \`TRUE\`). Overwrite \`file\` if it already exists.

## Value

A tibble of class \`cbe_review_tracker\` with columns \`item_id\`,
\`file\`, \`item_type\`, \`comment_id\`, \`author\`, \`paragraph\`,
\`selected_text\`, \`comment\`, \`trivial\`, \`status\`, \`location\`,
and \`rationale\`.

## See also

\[validate_review_tracker()\], \[cbe_docx_review_extract()\]
