# DOCX Review Extractor & Multi-Reviewer Collaborative Tracker

Extracts comments, tracked changes, and reconstructed paragraph redlines
directly from Microsoft Word (\`.docx\`) files and maintains a
collaborative review workflow in a formatted multi-sheet Excel workbook
and UTF-8 CSV files.

Supports multi-reviewer workflows with per-reviewer fork files
(\`review_tracker\_\<reviewer\>.xlsx\`), live Excel formulas, dropdown
data validations, conditional formatting, non-destructive merging across
document revisions, and analytical compute graph ordering.

## Usage

``` r
cbe_docx_review_extract(
  input_dir = NULL,
  output_dir = NULL,
  tracker = "review_tracker.xlsx",
  min_revision_length = 2,
  fork_reviewers = c("jka", "darina", "zhao"),
  manifest_path = NULL,
  verbose = FALSE
)
```

## Arguments

- input_dir:

  Character path to a directory containing \`.docx\` files, or to a
  single \`.docx\` file. Defaults to finding an edits directory or the
  current working directory.

- output_dir:

  Character path to the directory where review tracker workbooks and CSV
  files should be saved. If \`NULL\` (default), uses
  \`\<input_dir\>/review_extract\` (or \`\<input_dir\>\` if it is
  already named \`review_extract\`).

- tracker:

  Character filename for the master Excel workbook. Defaults to
  \`"review_tracker.xlsx"\`.

- min_revision_length:

  Integer minimum character count required to record a tracked change.
  Defaults to \`2\`.

- fork_reviewers:

  Character vector of reviewer identifiers who receive their own
  single-reviewer fork workbooks. Defaults to \`c("jka", "darina",
  "zhao")\`.

- manifest_path:

  Optional character path to \`reports_to_render.xlsx\`. If \`NULL\`,
  the function attempts to locate it automatically in standard
  repository paths.

- verbose:

  Logical indicating whether to print detailed progress messages.
  Defaults to \`FALSE\`.

## Value

An object of class \`cbe_review_extract\`, which is a named list
containing:

- comments:

  A tibble of merged comment records.

- revisions:

  A tibble of merged raw tracked changes.

- redlines:

  A tibble of reconstructed paragraph before/after redlines.

- documents:

  A tibble of document-level summary metrics and review statuses.

- docxwalk:

  A tibble crosswalking reviewed DOCX files to source \`.qmd\` and
  rendered outputs.

- summary:

  A tibble summarizing recurring comments across documents.

- errors:

  A tibble of any processing errors encountered.

- paths:

  A named list of generated file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- cbe_docx_review_extract("tasks/edits", verbose = TRUE)
print(res)
} # }
```
