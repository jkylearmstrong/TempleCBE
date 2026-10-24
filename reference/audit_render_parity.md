# Audit a Rendered PDF Against the Word Version of the Same Report

A lightweight check, run before deliverables are sent out, for the
differences that reviewers keep finding between the `.pdf` and the
`.docx` that Quarto renders from one source. It reads both files and
reports, as a table of findings:

- `margin_overflow`:

  words in the PDF that come closer than `margin_in` inches to the left
  or right edge of the page (a formula or a table that runs out of the
  text area).

- `numbering`:

  table or figure numbers that are missing from one of the two files or
  that are not consecutive in one of them.

- `order`:

  table and figure captions that come in another order in the PDF than
  in the Word file.

- `caption_placement`:

  a table caption that is above the table in one file and below it in
  the other.

- `numbers`:

  percentages and decimal numbers that occur in one file and not in the
  other (for example `98.9%` in the PDF and `97%` in Word), counted, so
  a number that occurs twice in one file and once in the other is
  reported.

A check that could not be made (a table that could not be found, or
found in more than one place, in the PDF) is listed as `info`.

## Usage

``` r
audit_render_parity(
  pdf_path,
  docx_path,
  margin_in = 0.5,
  check = c("margins", "numbering", "order", "placement", "numbers"),
  docx_limits = NULL
)
```

## Arguments

- pdf_path:

  Path to the rendered `.pdf`.

- docx_path:

  Path to the rendered `.docx`.

- margin_in:

  Minimum distance, in inches, between text and the left and right edge
  of a PDF page (default 0.5).

- check:

  Which checks to run: any of `"margins"`, `"numbering"`, `"order"`,
  `"placement"`, `"numbers"`. Default all.

- docx_limits:

  `NULL` or a named list of limits on the Word archive; see
  [`docx_review_extract()`](https://jkylearmstrong.github.io/TempleCBE/reference/docx_review_extract.md).

## Value

A tibble of class `render_parity` with the columns `check`, `severity`
(`"warning"` or `"info"`), `location` (for example `"page 3"` or
`"Table 2"`) and `detail`. No rows means nothing was found. Attributes:
`n_pages`, `captions` (the captions found in each file).

## Details

This reads text and positions only; it does not look at the pages. It is
meant to point at places to look at, not to certify a document. It was
checked against reports rendered by Quarto (PDF through LaTeX, Word
through pandoc); Word files written by hand in Word, other PDF engines
and two-column layouts are less tested.

- A caption is a line (PDF) or paragraph (Word) that begins with
  `Table`, `Figure` or `Fig.` (any case), a number, and a `.`, `:`, en
  dash or em dash followed by a space. A sentence that begins "Table 1
  shows" or "Table 1.5 mg were given" is not a caption. Quarto's
  non-breaking space after the word is read as a space. Supplementary
  numbers (`Table S1`) and panels (`Figure 1A`) are not recognised. A
  caption repeated on the continuation pages of a long table counts
  once.

- Quarto wraps each cross-referenced table and figure in a one-cell
  layout table; the wrapper is looked through. A table is a table of the
  Word file when it has at least two cells and no table inside it.
  Content controls are read.

- The place of a table in the PDF is found from the first row of its
  header, within a page of its caption, and not from a line that another
  caption is nearer to. A header that occurs in two places near the
  caption, or a table whose first cell is empty, gives an `info` row.

- Not read: text boxes, headers and footers of the Word file. Footnotes
  and endnotes of the Word file are read, because the PDF holds them in
  its text. Lines of the PDF that repeat on at least half of its pages
  (running headers and footers), the lines of its contents lists (a line
  that ends in dot leaders and a page number) and section numbers at the
  start of a line are left out of the comparison of numbers; so are the
  contents paragraphs of the Word file.

- Pages that the PDF stores rotated (landscape pages made by `pdflscape`
  and similar) are measured in their displayed width. Two-column layouts
  are not understood: lines are read across the columns, so captions in
  the second column can be missed.

- PDF text is as extracted by pdftools: a number that the PDF engine
  splits across lines can appear as a difference. Positions are whole
  points, so `margin_in` has a tolerance of 2 points.

The Word file is an untrusted archive and is read with the same size
limits as
[`docx_review_extract()`](https://jkylearmstrong.github.io/TempleCBE/reference/docx_review_extract.md)
(`docx_limits`); no entity is expanded and no network is used.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- audit_render_parity("report.pdf", "report.docx")
res
} # }
```
