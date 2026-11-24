# Convert a Single PDF to DOCX Using the Best Available Backend

Convenience wrapper around \[convert_pdfs_to_docx()\] for one file. Its
`src, dest` signature fits the `docx_from_pdf` callback of
\[zip_reports()\].

## Usage

``` r
convert_pdf_to_docx(
  src,
  dest = sub("\\.pdf$", ".docx", src, ignore.case = TRUE),
  backend = "auto",
  strict = TRUE,
  timeout = getOption("templecbe.docx_timeout", 600)
)
```

## Arguments

- src:

  Path to the input PDF file.

- dest:

  Path for the output DOCX file. Defaults to `src` with `.pdf` replaced
  by `.docx`.

- backend:

  One of `"auto"`, `"python"`, `"libreoffice"`, `"word_com"`.

- strict:

  If `TRUE` (default), stop at the first bad row and, when every row is
  fine, error with an actionable message if no backend is available. Set
  `FALSE` to warn and skip instead; a skipped row comes back with
  `converted = FALSE`.

- timeout:

  Seconds a converter may take on one file before it is stopped. A
  single number applies to every backend (python, LibreOffice and Word
  COM); a named vector such as `c(python = 120, libreoffice = 600)` sets
  each backend's own limit (a backend it does not name gets 600 s), so a
  heavy PDF can fail over from pdf2docx to LibreOffice sooner. The
  default is the `templecbe.docx_timeout` option, else 600, so
  `options(templecbe.docx_timeout = ...)` also reaches the conversions
  that \[render_pipeline()\] and \[zip_reports()\] make. A file that
  times out gets a warning that says so and comes back with
  `converted = FALSE` (the python backend then retries it once with
  LibreOffice, under LibreOffice's own `timeout`). Stopping a converter
  stops the process that was started; any process that one started in
  turn (LibreOffice's `soffice.bin`, or the Word instance behind Word
  COM) may keep running and has to be closed by hand.

## Value

Invisibly, `dest` on success, or `FALSE` if conversion failed.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_pdf_to_docx("report.pdf")
zip_reports(reports, output_formats = c("pdf", "docx"), docx_from_pdf = convert_pdf_to_docx)
} # }
```
