# Convert a PDF's Text to Rich Text Format (RTF)

Extracts the text layer of a PDF with \[pdftools::pdf_text()\] and
writes it to an RTF file in a monospaced font, one RTF page per PDF
page, so the column alignment of text tables (such as SAS listings) is
kept.

## Usage

``` r
pdf_to_rtf(
  pdf,
  rtf = sub("\\.pdf$", ".rtf", pdf, ignore.case = TRUE),
  font_size = 10,
  overwrite = TRUE
)
```

## Arguments

- pdf:

  Path to the input \`.pdf\` file.

- rtf:

  Path of the \`.rtf\` file to write. Defaults to \`pdf\` with its
  extension replaced by \`.rtf\`.

- font_size:

  Font size in points.

- overwrite:

  If \`FALSE\`, error when \`rtf\` already exists.

## Value

The path to \`rtf\`, invisibly.

## Details

Only text is converted: images, fonts, and vector graphics are not. A
scanned PDF with no text layer gives empty pages. Non-ASCII characters
are written as RTF Unicode escapes, so the file is plain ASCII and opens
the same in any locale.

## See also

\[create_toc_from_sas_pdf()\]

## Examples

``` r
pdf <- system.file("templates", "example.pdf", package = "TempleCBE")
rtf <- pdf_to_rtf(pdf, tempfile(fileext = ".rtf"))
readLines(rtf, n = 3)
#> [1] "{\\rtf1\\ansi\\ansicpg1252\\deff0"                 
#> [2] "{\\fonttbl{\\f0\\fmodern\\fcharset0 Courier New;}}"
#> [3] "\\viewkind4\\uc1\\pard\\f0\\fs20"                  
```
