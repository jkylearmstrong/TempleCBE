# Get or set global multi-reviewer review-tracking configuration

Decouples project-specific reviewer identities from the review-tracking
mechanics in
[`cbe_docx_review_extract`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_docx_review_extract.md),
mirroring how
[`pipeline_config`](https://jkylearmstrong.github.io/TempleCBE/reference/pipeline_config.md)
decouples study name and stage labels from the core compute-graph
classes: the number and names of reviewers are data supplied by the
calling project, not hardcoded here.

## Usage

``` r
review_config(
  fork_reviewers = NULL,
  signoff_reviewers = NULL,
  documents_sheet_mode = NULL,
  csv_reviewer_columns = NULL,
  pipeline_catalog = NULL,
  stem_aliases = NULL,
  analysis_dir = NULL,
  input_dirs = NULL,
  reset = FALSE
)
```

## Arguments

- fork_reviewers:

  Character vector of fork-reviewer identifiers. An id is used as a
  column name, as a fork-workbook file suffix and (with
  `documents_sheet_mode = "per_reviewer"`) in a sheet name, so it is
  validated: ids must be unique (ignoring case) and consist of 1-20
  letters, digits or underscores; they must not be the name of a
  built-in tracker column (for example `file`, `author` or `resolved`)
  and must not end in `_comment`. A violation is an error, and nothing
  is changed.

- signoff_reviewers:

  Character vector of document-level sign-off reviewer identifiers (same
  rules as `fork_reviewers`; an id cannot be both a fork and a sign-off
  reviewer). Defaults to none.

- documents_sheet_mode:

  One of `"columns"` (default; a single "Documents" sheet with one
  column, or column pair, per reviewer) or `"per_reviewer"` (a shared
  "Documents" sheet with counts and `resolved` only, plus one additional
  "Documents\_\<id\>" sheet per reviewer carrying just that reviewer's
  status/comment columns). This applies to the master workbook; a fork
  workbook always has one plain "Documents" sheet.

- csv_reviewer_columns:

  Which reviewer columns the CSV exports (`comments.csv`,
  `tracked_changes.csv`, `suggested_changes.csv`) contain. `"fork"`
  (default) keeps the fork reviewers' sign-off and comment columns but
  leaves out the sign-off reviewers' columns and the Word `author`
  column; `"none"` leaves out every reviewer column and the author;
  `"all"` writes every column, including the sign-off reviewers' and the
  author, which is what the CSVs held before this option existed.

- pipeline_catalog:

  The analysis stages the reviewed documents belong to, used to label
  (`pipeline_stage`) and order documents. A list of stages, each a list
  with a `stem` (the document's file name without extension, date and
  reviewer initials, compared in lower case) and optionally `heading`,
  `name` and `stage` (the label shown; default
  `"Heading <heading>: <name>"`, else the name, else the stem); a data
  frame with those columns also works. The order of the list is the
  order of the documents. The package ships *no* stages: unless a
  catalog (or a `reports_to_render.xlsx` manifest in `analysis_dir`) is
  supplied, every document gets the stage `"Unknown / Extra"`. A file
  whose stem is not in the catalog is matched to the most similar stem
  when they are at least 60 percent alike. Use
  [`list()`](https://rdrr.io/r/base/list.html) to clear.

- stem_aliases:

  Named character vector of file-name stems (as typed, for example a
  recurring typo) and the catalog stems they stand for. Defaults to
  none.

- analysis_dir:

  Folder, relative to the working directory (or to the
  `TEMPLECBE_ANALYSIS_ROOT` environment variable) or absolute, that
  holds `reports_to_render.xlsx` and the `.qmd` sources shown on the
  docXwalk sheet. Defaults to none: no manifest or source index is
  looked up. Use `character(0)` to clear.

- input_dirs:

  Character vector of folders (relative to the working directory or
  `TEMPLECBE_ANALYSIS_ROOT`, or absolute) in which
  [`cbe_docx_review_extract`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_docx_review_extract.md)
  looks, in order, for `.docx` files when `input_dir` is not given.
  Defaults to none, so the working directory is used. Use `character(0)`
  to clear.

- reset:

  If `TRUE`, forget everything set earlier (every argument above) and
  return to the defaults before applying any other argument given in the
  same call; the arguments are validated first, so a rejected call
  changes nothing. `review_config(reset = TRUE)` alone just restores the
  defaults. Defaults to `FALSE`.

## Value

A list containing the current configuration options.

## Details

Two reviewer categories are supported:

- `fork_reviewers`: reviewers who sign off on individual comments and
  suggested changes. Each gets a personal fork workbook
  (`review_tracker_<id>.xlsx`) that holds only that reviewer's own
  columns: no other reviewer's columns, no sign-off reviewer's columns
  and no Word author names appear on any sheet, and the "Documents"
  sheet carries just the counts and that reviewer's own rolled-up column
  (there is no "Documents\_\<id\>" sheet in a fork). In a fork the
  `resolved` columns reflect that reviewer's own sign-offs only; in the
  master workbook a document's `resolved` status is TRUE only when every
  fork reviewer has signed off on every comment and suggested change in
  that document.

- `signoff_reviewers`: reviewers who sign off once per document (e.g. a
  final PI read), tracked on the master workbook's "Documents" sheet (a
  status column plus a free-text comment column). They are not counted
  toward `resolved`, and they never appear in a fork workbook or, by
  default, in the CSV exports (see `csv_reviewer_columns`). The master's
  Comments and SuggestedChanges sheets also carry their column pair, so
  that anything typed there is kept.

Existing `review_tracker_*.xlsx` files continue to work: column data is
preserved by header union regardless of configuration, but for the app's
reviewer-aware behavior (rollups, resolved status, fork filtering) to
line up with a file's existing columns, keep configuring the same
reviewer identifiers used when that file was created.
