# Report Which PDF -\> DOCX Backends Are Usable on This Machine

Safe to call at any time; performs discovery only and converts nothing.

## Usage

``` r
check_docx_toolchain(quiet = FALSE)
```

## Arguments

- quiet:

  Suppress the printed diagnosis.

## Value

Invisibly, a list with elements `python_with_pdf2docx`, `python_any`,
`soffice`, `word_com` and `backend`.

## See also

\[convert_pdf_to_docx()\], \[convert_pdfs_to_docx()\]

## Examples

``` r
if (FALSE) { # \dontrun{
check_docx_toolchain()
} # }
```
