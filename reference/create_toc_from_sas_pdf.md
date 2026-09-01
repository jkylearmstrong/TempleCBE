# Build a Table of Contents from a SAS-Generated PDF

Scans a PDF (as produced by SAS reporting output) for table titles and
writes a Table-of-Contents `.rtf` file alongside it, prefixed `TOC_`.

## Usage

``` r
create_toc_from_sas_pdf(input, collapse = FALSE, top_margin_height = 60)
```

## Arguments

- input:

  Path to the input PDF.

- collapse:

  If `FALSE` (default), every occurrence of a title is listed; if
  `TRUE`, only the first in a numbered sequence is kept.

- top_margin_height:

  How far down the page (in PDF text-position units) to look for titles.

## Value

Invisibly, a message string naming the output file path.

## Examples

``` r
if (FALSE) { # \dontrun{
create_toc_from_sas_pdf("path/to/report.pdf", collapse = TRUE)
} # }
```
