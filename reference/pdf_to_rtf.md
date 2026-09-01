# Convert a PDF to Rich Text Format (RTF)

Extracts text from a PDF via
[`pdf_text`](https://docs.ropensci.org/pdftools//reference/pdftools.html)
and writes it to a portable `.rtf` file.

## Usage

``` r
pdf_to_rtf(path_To_PDF_File, path_To_write_RTF_File)
```

## Arguments

- path_To_PDF_File:

  Path to the input `.pdf` file.

- path_To_write_RTF_File:

  Path to write the converted `.rtf` file to.

## Value

The normalized path to the created RTF file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
pdf_to_rtf("report.pdf", "report.rtf")
} # }
```
