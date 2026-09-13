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
  timeout = 600
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

  If `TRUE` (default), error with an actionable message when no backend
  is available. Set `FALSE` to warn and skip.

- timeout:

  Seconds before the Word COM backend gives up on a file.

## Value

Invisibly, `dest` on success, or `FALSE` if conversion failed.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_pdf_to_docx("report.pdf")
zip_reports(reports, output_formats = c("pdf", "docx"), docx_from_pdf = convert_pdf_to_docx)
} # }
```
