# Convert PDFs to DOCX Using the Best Available Backend

Backend preference is python (`pdf2docx`) -\> LibreOffice -\> Word COM
(Windows only). When the python backend fails on an individual file and
LibreOffice is present, that file is retried once with LibreOffice.

## Usage

``` r
convert_pdfs_to_docx(
  conversions,
  backend = "auto",
  strict = TRUE,
  timeout = 600
)
```

## Arguments

- conversions:

  A data frame with character columns `src` and `dest`, and optionally
  `temp_dest`, a second location each converted file is copied to.

- backend:

  One of `"auto"`, `"python"`, `"libreoffice"`, `"word_com"`.

- strict:

  If `TRUE` (default), stop at the first bad row and, when every row is
  fine, error with an actionable message if no backend is available. Set
  `FALSE` to warn and skip instead; a skipped row comes back with
  `converted = FALSE`.

- timeout:

  Seconds a converter may take on one file before it is stopped; it
  applies to every backend (python, LibreOffice and Word COM). A file
  that times out gets a warning that says so and comes back with
  `converted = FALSE` (the python backend then retries it once with
  LibreOffice, which gets its own `timeout`). Stopping a converter stops
  the process that was started; any process that one started in turn
  (LibreOffice's `soffice.bin`, or the Word instance behind Word COM)
  may keep running and has to be closed by hand.

## Value

`conversions` with an added logical `converted` column.

## Details

The rows of `conversions` are checked before any backend is looked for,
so a bad row is reported the same way on every machine, whether or not a
converter is installed. A row is bad when its `src` does not exist or is
empty (0 bytes), when its `dest` is missing, or when its `dest` or
`temp_dest` ends in `.pdf` (a DOCX is not a PDF, and the file that is
there would be overwritten). A `src` that resolves to the same file as
its `dest` or `temp_dest`, however the path is spelled (`./a.pdf`,
another case), and a `dest` that is the same file as its `temp_dest`,
are always errors, whatever `strict` is, because converting would
overwrite the PDF or empty the copy. Rows that pass are then converted;
if none pass, no backend is looked for at all.

A converter writes into a fresh file next to `dest`, and that file
replaces `dest` only when the conversion succeeded and left a non-empty
file. For pdf2docx and Word COM, succeeded means that the converter
exited with status 0. LibreOffice's exit status is not looked at: it
writes into a private, empty folder, so whatever it leaves there was
written by this run, and a first start with a fresh profile may exit
non-zero after a good conversion. A time-out is a failure for every
backend. A DOCX that was already at `dest` (from an earlier run, say) is
never mistaken for the result: when the conversion fails, `dest` is left
exactly as it was (an old file is neither deleted nor reported as
converted, and a half-written file from a crashed converter is
discarded), and the row comes back with `converted = FALSE`.

## See also

\[convert_pdf_to_docx()\], \[check_docx_toolchain()\], and
\[zip_reports()\], whose `docx_from_pdf` argument accepts
`convert_pdf_to_docx`.
