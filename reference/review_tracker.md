# Build a Structured Review Response Tracker Requiring Location and Rationale

Constructs a review response tracker table (and optional Excel workbook
or CSV) from the output of \[docx_review_extract()\]. Each comment (and
optionally each reconstructed redline) is given explicit \`status\`,
\`location\` (page, section, table, or figure where addressed), and
\`rationale\` (reason if not adopted, or summary of changes made)
columns for validation by \[validate_review_tracker()\].

## Usage

``` r
review_tracker(
  extract,
  file = NULL,
  include_redlines = FALSE,
  overwrite = TRUE,
  format = c("auto", "xlsx", "excel", "csv", "docx", "markdown")
)
```

## Arguments

- extract:

  An object of class \`cbe_review_extract\` from
  \[docx_review_extract()\], a data frame of extracted comments, or one
  or more paths, each a \`.docx\` file or a directory of \`.docx\` files
  (several paths are extracted one by one and their comments combined).

- file:

  Optional output path to write the tracker to: \`.xlsx\`, \`.csv\`,
  \`.docx\` or \`.md\`.

- include_redlines:

  Logical (default \`FALSE\`). When \`TRUE\` and \`extract\` is a
  \`cbe_review_extract\` object with non-empty \`\$redlines\`, includes
  tracked paragraph redlines in addition to comments.

- overwrite:

  Logical (default \`TRUE\`). Overwrite \`file\` if it already exists.

- format:

  How to write \`file\`: \`"auto"\` (default) takes it from the file
  extension (\`.xlsx\`, \`.docx\`, \`.md\` / \`.markdown\`; anything
  else is \`.csv\`), or \`"xlsx"\` / \`"excel"\`, \`"csv"\`, \`"docx"\`,
  \`"markdown"\`. A \`format\` other than \`"auto"\` needs \`file\`. See
  \[format_review_tracker()\] to format a tracker without extracting it
  again.

## Value

A tibble of class \`cbe_review_tracker\` with columns \`item_id\`,
\`file\`, \`item_type\`, \`comment_id\`, \`author\`, \`paragraph\`,
\`selected_text\`, \`comment\`, \`trivial\`, \`status\`, \`location\`,
and \`rationale\`.

## See also

\[validate_review_tracker()\], \[docx_review_extract()\]
