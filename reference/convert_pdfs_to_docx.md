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

  If `TRUE` (default), error with an actionable message when no backend
  is available. Set `FALSE` to warn and skip.

- timeout:

  Seconds before the Word COM backend gives up on a file.

## Value

`conversions` with an added logical `converted` column.

## See also

\[convert_pdf_to_docx()\], \[check_docx_toolchain()\], and
\[zip_reports()\], whose `docx_from_pdf` argument accepts
`convert_pdf_to_docx`.
